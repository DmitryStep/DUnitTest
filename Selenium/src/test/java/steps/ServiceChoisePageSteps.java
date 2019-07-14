package steps;

import baseclasses.BaseSteps;
import cucumber.api.java.After;
import cucumber.api.java.Before;
import cucumber.api.java.en.When;
import cucumber.api.java.ru.����;
import pageobjects.pages.ServiceChoisePage;

public class ServiceChoisePageSteps extends BaseSteps {

    private ServiceChoisePage _serviceChoisePage = null;

    //----------------------------------------- Hooks -----------------------------------------------------

    @Before(order = 3)
    public void beforeTest(){
        _serviceChoisePage = new ServiceChoisePage(_driver, _waiter);
    }

    @After(order = 3)
    public void afterTest() {
        AfterTest();
        _serviceChoisePage = null;
    }

    //----------------------------------------- Steps -----------------------------------------------------

    @����("������ ILS Plan")
    @When("Click ILS Plan")
    public void clickILSPlan() {
        _serviceChoisePage.clickILSPlanButton();
    }

    @����("������ ILS Fact")
    @When("Click ILS Fact")
    public void clickILSFact() {
        _serviceChoisePage.clickILSFactButton();
    }

    @����("������ ILS Admin")
    @When("Click ILS Admin")
    public void clickILSAdmin() {
        _serviceChoisePage.clickILSAdminButton();
    }

    @����("������ ILS Oper")
    @When("Click ILS Oper")
    public void clickILSOper() {
        _serviceChoisePage.clickILSOperButton();
    }


}
