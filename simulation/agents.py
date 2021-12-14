import datetime
import math
import random
from typing import TYPE_CHECKING, Dict, Set

import pandas as pd

from abm import Agent

if TYPE_CHECKING:
    from simulation.model import DomesticHeatingABM

from simulation.constants import (
    BOILERS,
    DISCOUNT_RATE_WEIBULL_ALPHA,
    DISCOUNT_RATE_WEIBULL_BETA,
    FLOOR_AREA_SQM_33RD_PERCENTILE,
    FLOOR_AREA_SQM_66TH_PERCENTILE,
    FUEL_KWH_TO_HEAT_KWH,
    GB_PROPERTY_VALUE_WEIBULL_ALPHA,
    GB_PROPERTY_VALUE_WEIBULL_BETA,
    GB_RENOVATION_BUDGET_WEIBULL_ALPHA,
    GB_RENOVATION_BUDGET_WEIBULL_BETA,
    HAZARD_RATE_HEATING_SYSTEM_ALPHA,
    HAZARD_RATE_HEATING_SYSTEM_BETA,
    HEAT_PUMP_CAPACITY_SCALE_FACTOR,
    HEAT_PUMPS,
    HEATING_KWH_PER_SQM_ANNUAL,
    HEATING_PROPORTION_OF_RENO_BUDGET,
    HEATING_SYSTEM_FUEL,
    MAX_HEAT_PUMP_CAPACITY_KW,
    MIN_HEAT_PUMP_CAPACITY_KW,
    RENO_NUM_INSULATION_ELEMENTS_UPGRADED,
    RENO_PROBA_HEATING_SYSTEM_UPDATE,
    RENO_PROBA_INSULATION_UPDATE,
    RETROFIT_COSTS_SMALL_PROPERTY_SQM_LIMIT,
    BuiltForm,
    ConstructionYearBand,
    Element,
    Epc,
    EventTrigger,
    HeatingFuel,
    HeatingSystem,
    InsulationSegment,
    InterventionType,
    OccupantType,
    PropertySize,
    PropertyType,
)
from simulation.costs import (
    CAVITY_WALL_INSULATION_COST,
    DOUBLE_GLAZING_UPVC_COST,
    HEATING_FUEL_PRICE_GBP_PER_KWH,
    INTERNAL_WALL_INSULATION_COST,
    LOFT_INSULATION_JOISTS_COST,
    discount_annual_cash_flow,
    estimate_rhi_annual_payment,
    get_heating_fuel_costs_net_present_value,
    get_unit_and_install_costs,
)


def sample_interval_uniformly(interval: pd.Interval) -> float:
    return random.randint(interval.left, interval.right)


class Household(Agent):
    def __init__(
        self,
        location: str,
        property_value_gbp: int,
        floor_area_sqm: int,
        off_gas_grid: bool,
        construction_year_band: ConstructionYearBand,
        property_type: PropertyType,
        built_form: BuiltForm,
        heating_system: HeatingSystem,
        heating_system_install_date: datetime.date,
        epc: Epc,
        potential_epc: Epc,
        occupant_type: OccupantType,
        is_solid_wall: bool,
        walls_energy_efficiency: int,
        windows_energy_efficiency: int,
        roof_energy_efficiency: int,
        is_heat_pump_suitable_archetype: bool,
        is_heat_pump_aware: bool,
    ):
        # Property / tenure attributes
        self.location = location
        self.property_type = property_type
        self.occupant_type = occupant_type
        self.built_form = built_form
        self.floor_area_sqm = floor_area_sqm
        self.property_value_gbp = property_value_gbp
        self.is_solid_wall = is_solid_wall
        self.construction_year_band = construction_year_band
        self.is_heat_pump_suitable_archetype = is_heat_pump_suitable_archetype

        # Heating / energy performance attributes
        self.off_gas_grid = off_gas_grid
        self.heating_functioning = True
        self.heating_system = heating_system
        self.heating_system_install_date = heating_system_install_date
        self.epc = epc
        self.potential_epc = potential_epc
        self.walls_energy_efficiency = walls_energy_efficiency
        self.roof_energy_efficiency = roof_energy_efficiency
        self.windows_energy_efficiency = windows_energy_efficiency
        self.is_heat_pump_aware = is_heat_pump_aware

        # Household investment decision attributes
        self.is_renovating = False
        self.renovate_insulation = False
        self.renovate_heating_system = False
        self.epc_c_upgrade_costs = {}
        self.heating_system_total_costs = {}

    @property
    def heating_fuel(self) -> HeatingFuel:
        return HEATING_SYSTEM_FUEL[self.heating_system]

    @staticmethod
    def get_weibull_percentile_from_value(
        alpha: float, beta: float, input_value: float
    ) -> float:

        return 1 - math.exp(-((input_value / beta) ** alpha))

    @staticmethod
    def get_weibull_value_from_percentile(
        alpha: float, beta: float, percentile: float
    ) -> float:

        epsilon = 0.0000001
        return beta * (-math.log(1 + epsilon - percentile)) ** (1 / alpha)

    @staticmethod
    def true_with_probability(p: float) -> bool:
        return random.random() < p

    @staticmethod
    def weibull_hazard_rate(alpha: float, beta: float, age_years: float) -> float:
        # Source: # https://en.wikipedia.org/wiki/Weibull_distribution

        """
        alpha: A value > 1 indicates that failure rates increases over time (e.g. an ageing process)
        beta: The larger this value, the more 'spread out' the distribution is
        age_years: The age of an item subject to failures over time (e.g. heating_type, or vehicle)
        """

        return (alpha / beta) * (age_years / beta) ** (alpha - 1)

    @property
    def wealth_percentile(self) -> float:

        return self.get_weibull_percentile_from_value(
            GB_PROPERTY_VALUE_WEIBULL_ALPHA,
            GB_PROPERTY_VALUE_WEIBULL_BETA,
            self.property_value_gbp,
        )

    @property
    def discount_rate(self) -> float:

        return max(
            1
            - self.get_weibull_value_from_percentile(
                DISCOUNT_RATE_WEIBULL_ALPHA,
                DISCOUNT_RATE_WEIBULL_BETA,
                self.wealth_percentile,
            ),
            0,
        )

    @property
    def renovation_budget(self) -> float:

        return (
            HEATING_PROPORTION_OF_RENO_BUDGET
            * self.get_weibull_value_from_percentile(
                GB_RENOVATION_BUDGET_WEIBULL_ALPHA,
                GB_RENOVATION_BUDGET_WEIBULL_BETA,
                self.wealth_percentile,
            )
        )

    @property
    def insulation_segment(self) -> InsulationSegment:

        if self.property_type == PropertyType.FLAT:
            if self.floor_area_sqm < RETROFIT_COSTS_SMALL_PROPERTY_SQM_LIMIT["FLAT"]:
                return InsulationSegment.SMALL_FLAT
            return InsulationSegment.LARGE_FLAT

        if (
            self.property_type == PropertyType.HOUSE
            and self.built_form == BuiltForm.MID_TERRACE
        ):
            return (
                InsulationSegment.SMALL_MID_TERRACE_HOUSE
                if self.floor_area_sqm
                < RETROFIT_COSTS_SMALL_PROPERTY_SQM_LIMIT["MID_TERRACE_HOUSE"]
                else InsulationSegment.LARGE_MID_TERRACE_HOUSE
            )

        if self.property_type == PropertyType.HOUSE and self.built_form in [
            BuiltForm.END_TERRACE,
            BuiltForm.SEMI_DETACHED,
        ]:
            return (
                InsulationSegment.SMALL_SEMI_END_TERRACE_HOUSE
                if self.floor_area_sqm
                < RETROFIT_COSTS_SMALL_PROPERTY_SQM_LIMIT["SEMI_OR_END_TERRACE_HOUSE"]
                else InsulationSegment.LARGE_SEMI_END_TERRACE_HOUSE
            )

        if (
            self.property_type == PropertyType.HOUSE
            and self.built_form == BuiltForm.DETACHED
        ):
            return (
                InsulationSegment.SMALL_DETACHED_HOUSE
                if self.floor_area_sqm
                < RETROFIT_COSTS_SMALL_PROPERTY_SQM_LIMIT["SMALL_DETACHED_HOUSE"]
                else InsulationSegment.LARGE_DETACHED_HOUSE
            )

        if self.property_type == PropertyType.BUNGALOW:
            return InsulationSegment.BUNGALOW

    @property
    def is_heat_pump_suitable(self) -> bool:

        return (
            False
            if not all(
                [
                    self.is_heat_pump_suitable_archetype,
                    self.potential_epc.value >= Epc.C.value,
                ]
            )
            else True
        )

    @property
    def property_size(self) -> PropertySize:

        if self.floor_area_sqm < FLOOR_AREA_SQM_33RD_PERCENTILE:
            return PropertySize.SMALL
        elif self.floor_area_sqm > FLOOR_AREA_SQM_66TH_PERCENTILE:
            return PropertySize.LARGE
        else:
            return PropertySize.MEDIUM

    @property
    def annual_kwh_heating_demand(self) -> float:

        return (
            self.floor_area_sqm * HEATING_KWH_PER_SQM_ANNUAL
        ) / FUEL_KWH_TO_HEAT_KWH[self.heating_system]

    @property
    def annual_heating_fuel_bill(self) -> int:

        return int(
            self.annual_kwh_heating_demand
            * HEATING_FUEL_PRICE_GBP_PER_KWH[HEATING_SYSTEM_FUEL[self.heating_system]]
        )

    def heating_system_age_years(self, current_date: datetime.date) -> float:
        return (current_date - self.heating_system_install_date).days / 365

    def is_heating_system_hassle(self, heating_system: HeatingSystem) -> bool:
        if heating_system in BOILERS or self.heating_system == heating_system:
            return False
        return True

    def evaluate_renovation(self, model) -> None:

        step_interval_years = model.step_interval / datetime.timedelta(days=365)
        proba_renovate = model.annual_renovation_rate * step_interval_years

        self.is_renovating = self.true_with_probability(proba_renovate)

        self.renovate_heating_system = (
            self.true_with_probability(RENO_PROBA_HEATING_SYSTEM_UPDATE)
            if self.is_renovating
            else False
        )
        self.renovate_insulation = (
            self.true_with_probability(RENO_PROBA_INSULATION_UPDATE)
            if self.is_renovating
            else False
        )

    def get_upgradable_insulation_elements(self) -> Set[Element]:

        measures_and_grades = zip(
            [Element.WALLS, Element.ROOF, Element.GLAZING],
            [
                self.walls_energy_efficiency,
                self.roof_energy_efficiency,
                self.windows_energy_efficiency,
            ],
        )

        MAX_ENERGY_EFFICIENCY_SCORE = 5
        return {
            measure
            for measure, grade in measures_and_grades
            if grade < MAX_ENERGY_EFFICIENCY_SCORE
        }

    def get_num_insulation_elements(self, event_trigger: EventTrigger) -> int:

        if event_trigger == EventTrigger.RENOVATION:
            return random.choices(
                list(RENO_NUM_INSULATION_ELEMENTS_UPGRADED.keys()),
                weights=RENO_NUM_INSULATION_ELEMENTS_UPGRADED.values(),
            )[0]

        if event_trigger == EventTrigger.EPC_C_UPGRADE:
            # The number of insulation elements a household would require to reach epc C
            # We assume each insulation measure will contribute +1 EPC grade
            return max(0, Epc.C.value - self.epc.value)

        return 0

    def get_quote_insulation_elements(
        self, elements: Set[Element]
    ) -> Dict[Element, float]:

        insulation_quotes = {element: 0 for element in elements}
        for element in elements:
            if element == Element.WALLS:
                if self.is_solid_wall:
                    cost_range = INTERNAL_WALL_INSULATION_COST[self.insulation_segment]
                    insulation_quotes[element] = sample_interval_uniformly(cost_range)
                else:
                    cost_range = CAVITY_WALL_INSULATION_COST[self.insulation_segment]
                    insulation_quotes[element] = sample_interval_uniformly(cost_range)
            if element == Element.GLAZING:
                cost_range = DOUBLE_GLAZING_UPVC_COST[self.insulation_segment]
                insulation_quotes[element] = sample_interval_uniformly(cost_range)
            if element == Element.ROOF:
                cost_range = LOFT_INSULATION_JOISTS_COST[self.insulation_segment]
                insulation_quotes[element] = sample_interval_uniformly(cost_range)

        return insulation_quotes

    def choose_insulation_elements(
        self, insulation_quotes: Dict[Element, float], num_elements: int
    ) -> Dict[Element, float]:

        return {
            element: insulation_quotes[element]
            for element in sorted(insulation_quotes, key=insulation_quotes.get)[
                :num_elements
            ]
        }

    def install_insulation_elements(
        self, insulation_elements: Dict[Element, float]
    ) -> None:

        for element in insulation_elements:
            if element == Element.ROOF:
                self.roof_energy_efficiency = 5
            if element == Element.WALLS:
                self.walls_energy_efficiency = 5
            if element == Element.GLAZING:
                self.windows_energy_efficiency = 5

        n_measures = len(insulation_elements)
        improved_epc_level = min(6, self.epc.value + n_measures)
        self.epc = Epc(improved_epc_level)

    def get_chosen_insulation_costs(self, event_trigger: EventTrigger):

        upgradable_elements = self.get_upgradable_insulation_elements()
        insulation_quotes = self.get_quote_insulation_elements(upgradable_elements)

        num_elements = min(
            len(upgradable_elements), self.get_num_insulation_elements(event_trigger)
        )

        return self.choose_insulation_elements(insulation_quotes, num_elements)

    def get_heating_system_options(
        self, model: "DomesticHeatingABM", event_trigger: EventTrigger
    ) -> Set[HeatingSystem]:

        heating_system_options = model.heating_systems.copy()
        if not self.is_heat_pump_suitable or not self.is_heat_pump_aware:
            heating_system_options -= HEAT_PUMPS

        if self.off_gas_grid:
            heating_system_options -= {HeatingSystem.BOILER_GAS}
        else:
            heating_system_options -= {HeatingSystem.BOILER_OIL}

        if self.property_size != PropertySize.SMALL:
            heating_system_options -= {HeatingSystem.BOILER_ELECTRIC}

        if event_trigger == EventTrigger.BREAKDOWN:
            # if household already has a heat pump, they can reinstall that type of heat pump in a breakdown
            # heat pumps are otherwise unfeasible in a breakdown due to installation lead times
            unfeasible_heating_systems = HEAT_PUMPS - {self.heating_system}
            heating_system_options -= unfeasible_heating_systems

        return heating_system_options

    def get_total_heating_system_costs(
        self,
        heating_system: HeatingSystem,
        model: "DomesticHeatingABM",
    ):

        unit_and_install_costs = get_unit_and_install_costs(self, heating_system, model)
        fuel_costs_net_present_value = get_heating_fuel_costs_net_present_value(
            self, heating_system, model.household_num_lookahead_years
        )

        if model.intervention == InterventionType.RHI:
            rhi_annual_payment = estimate_rhi_annual_payment(self, heating_system)
            subsidies = discount_annual_cash_flow(
                discount_rate=self.discount_rate,
                cashflow_gbp=rhi_annual_payment,
                duration_years=7,
            )
        else:
            subsidies = 0

        return unit_and_install_costs + fuel_costs_net_present_value - subsidies

    def choose_heating_system(
        self, costs: Dict[HeatingSystem, float], heating_system_hassle_factor: float
    ):

        weights = []

        for heating_system in costs.keys():
            weight = 1 / (1 + math.exp(costs[heating_system] / self.renovation_budget))
            if self.is_heating_system_hassle(heating_system):
                weight *= 1 - heating_system_hassle_factor
            weights.append(weight)

        return random.choices(list(costs.keys()), weights)[0]

    def install_heating_system(
        self, heating_system: HeatingSystem, model: "DomesticHeatingABM"
    ) -> None:

        self.heating_system = heating_system
        self.heating_system_install_date = model.current_datetime.date()
        self.heating_functioning = True

    def update_heating_status(self, model: "DomesticHeatingABM") -> None:

        self.epc_c_upgrade_costs = {}
        self.heating_system_total_costs = {}

        step_interval_years = model.step_interval / datetime.timedelta(days=365)
        probability_density = self.weibull_hazard_rate(
            HAZARD_RATE_HEATING_SYSTEM_ALPHA,
            HAZARD_RATE_HEATING_SYSTEM_BETA,
            self.heating_system_age_years(model.current_datetime.date()),
        )
        proba_failure = probability_density * step_interval_years
        if random.random() < proba_failure:
            self.heating_functioning = False

    def compute_heat_pump_capacity_kw(self, heat_pump_type: HeatingSystem) -> int:

        capacity_kw = (
            HEAT_PUMP_CAPACITY_SCALE_FACTOR[heat_pump_type] * self.floor_area_sqm
        )
        return math.ceil(
            min(
                max(capacity_kw, MIN_HEAT_PUMP_CAPACITY_KW[heat_pump_type]),
                MAX_HEAT_PUMP_CAPACITY_KW[heat_pump_type],
            )
        )

    def make_decisions(self, model):

        self.update_heating_status(model)
        self.evaluate_renovation(model)

        if self.is_renovating:
            if self.renovate_insulation:
                chosen_elements = self.get_chosen_insulation_costs(
                    event_trigger=EventTrigger.RENOVATION
                )
                self.install_insulation_elements(chosen_elements)

        if not self.heating_functioning or (
            self.is_renovating and self.renovate_heating_system
        ):

            if not self.heating_functioning:
                heating_system_options = self.get_heating_system_options(
                    model, event_trigger=EventTrigger.BREAKDOWN
                )
            else:
                heating_system_options = self.get_heating_system_options(
                    model, event_trigger=EventTrigger.RENOVATION
                )
            chosen_insulation_costs = self.get_chosen_insulation_costs(
                event_trigger=EventTrigger.EPC_C_UPGRADE
            )

            costs = {}
            for heating_system in heating_system_options:
                costs[heating_system] = self.get_total_heating_system_costs(
                    heating_system, model
                )
                if heating_system in HEAT_PUMPS:
                    costs[heating_system] += sum(chosen_insulation_costs.values())

            self.heating_system_total_costs = costs
            self.epc_c_upgrade_costs = chosen_insulation_costs

            chosen_heating_system = self.choose_heating_system(
                costs, model.heating_system_hassle_factor
            )

            self.install_heating_system(chosen_heating_system, model)
            if chosen_heating_system in HEAT_PUMPS:
                upgraded_insulation_elements = chosen_insulation_costs.keys()
                self.install_insulation_elements(upgraded_insulation_elements)