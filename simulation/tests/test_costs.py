import datetime
import random

import pytest
from dateutil.relativedelta import relativedelta

from simulation.constants import BOILERS, HEAT_PUMPS, HeatingSystem
from simulation.costs import (
    estimate_rhi_annual_payment,
    get_heating_fuel_costs_net_present_value,
    get_unit_and_install_costs,
)
from simulation.tests.common import household_factory, model_factory


class TestCosts:
    @pytest.mark.parametrize("heating_system", set(HeatingSystem))
    def test_cost_of_any_heating_system_is_cheaper_if_already_installed(
        self, heating_system
    ) -> None:
        household_sticking_same_system = household_factory(
            heating_system=heating_system
        )

        alternative_system = random.choice(list(set(HeatingSystem) - {heating_system}))
        household_switching_system = household_factory(
            heating_system=alternative_system
        )

        model = model_factory()

        assert get_unit_and_install_costs(
            household_sticking_same_system, heating_system, model
        ) < get_unit_and_install_costs(
            household_switching_system, heating_system, model
        )

    @pytest.mark.parametrize("heat_pump", HEAT_PUMPS)
    def test_cost_of_heat_pump_increases_with_kw_capacity_required(
        self,
        heat_pump,
    ) -> None:

        household = household_factory(
            floor_area_sqm=random.randint(20, 200), heating_system=heat_pump
        )
        larger_household = household_factory(
            floor_area_sqm=household.floor_area_sqm * 1.2,
            heating_system=heat_pump,
        )

        model = model_factory()

        assert household.compute_heat_pump_capacity_kw(
            heat_pump
        ) <= larger_household.compute_heat_pump_capacity_kw(heat_pump)
        assert get_unit_and_install_costs(
            household, heat_pump, model
        ) <= get_unit_and_install_costs(larger_household, heat_pump, model)

    @pytest.mark.parametrize("boiler", BOILERS)
    def test_cost_of_boiler_increases_with_property_size(
        self,
        boiler,
    ) -> None:
        household = household_factory(
            floor_area_sqm=random.randint(20, 200), heating_system=boiler
        )
        larger_household = household_factory(
            floor_area_sqm=household.floor_area_sqm * 1.5, heating_system=boiler
        )
        model = model_factory()
        assert get_unit_and_install_costs(
            household, boiler, model
        ) <= get_unit_and_install_costs(larger_household, boiler, model)

    @pytest.mark.parametrize("heating_system", set(HeatingSystem))
    def test_fuel_bills_net_present_value_decreases_as_discount_rate_increases(
        self,
        heating_system,
    ) -> None:

        num_look_ahead_years = random.randint(2, 10)
        household = household_factory(
            property_value_gbp=random.randint(50_000, 300_000)
        )
        wealthier_household = household_factory(
            property_value_gbp=household.property_value_gbp * 1.1
        )

        assert household.discount_rate > wealthier_household.discount_rate

        assert get_heating_fuel_costs_net_present_value(
            household, heating_system, num_look_ahead_years
        ) < get_heating_fuel_costs_net_present_value(
            wealthier_household, heating_system, num_look_ahead_years
        )

    @pytest.mark.parametrize("heat_pump", set(HEAT_PUMPS))
    def test_heat_pumps_are_cheaper_to_reinstall_than_install_first_time(
        self,
        heat_pump,
    ) -> None:

        household = household_factory(heating_system=HeatingSystem.BOILER_GAS)
        model = model_factory()

        new_heat_pump_quote = get_unit_and_install_costs(household, heat_pump, model)

        household.heating_system = heat_pump
        reinstall_heat_pump_quote = get_unit_and_install_costs(
            household, heat_pump, model
        )

        assert reinstall_heat_pump_quote < new_heat_pump_quote

    @pytest.mark.parametrize("heat_pump", set(HEAT_PUMPS))
    def test_rhi_annual_payments_are_non_zero_for_households_switching_to_heat_pumps(
        self, heat_pump
    ):

        household_with_boiler = household_factory(
            heating_system=random.choices(list(BOILERS))[0]
        )

        assert estimate_rhi_annual_payment(household_with_boiler, heat_pump) > 0

    @pytest.mark.parametrize("boiler", set(BOILERS))
    def test_rhi_annual_payments_zero_for_households_switching_to_boilers(self, boiler):

        household = household_factory(
            heating_system=random.choices(list(HeatingSystem))[0]
        )

        assert estimate_rhi_annual_payment(household, boiler) == 0

    @pytest.mark.parametrize("heat_pump", set(HEAT_PUMPS))
    def test_rhi_annual_payments_reach_cap_for_large_households(self, heat_pump):

        mansion = household_factory(
            heating_system=random.choices(list(BOILERS))[0],
            floor_area_sqm=random.randint(500, 1_000),
        )

        larger_mansion = household_factory(
            heating_system=mansion.heating_system,
            floor_area_sqm=mansion.floor_area_sqm * 1.1,
        )

        assert estimate_rhi_annual_payment(
            mansion, heat_pump
        ) == estimate_rhi_annual_payment(larger_mansion, heat_pump)

    def test_air_source_heat_pumps_get_cheaper_across_2022(self):

        household = household_factory(heating_system=HeatingSystem.HEAT_PUMP_AIR_SOURCE)
        model = model_factory(
            start_datetime=datetime.datetime(2022, 1, 1, 0, 0),
            air_source_heat_pump_discount_factor_2022=0.3,
        )
        quote = get_unit_and_install_costs(
            household, HeatingSystem.HEAT_PUMP_AIR_SOURCE, model
        )

        for n in range(1, 24):
            model.current_datetime += relativedelta(months=1)
            future_quote = get_unit_and_install_costs(
                household, HeatingSystem.HEAT_PUMP_AIR_SOURCE, model
            )
            if n < 12:
                assert quote > future_quote
            if n >= 12:
                assert quote == future_quote

            quote = future_quote