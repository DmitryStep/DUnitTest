package steps;

import baseclasses.BaseSteps;
import cucumber.api.java.After;
import cucumber.api.java.Before;
import cucumber.api.java.en.When;
import cucumber.api.java.ru.Если;
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

    @Если("Нажать ILS Plan")
    @When("Click ILS Plan")
    public void clickILSPlan() {
        _serviceChoisePage.clickILSPlanButton();
    }

    @Если("Нажать ILS Fact")
    @When("Click ILS Fact")
    public void clickILSFact() {
        _serviceChoisePage.clickILSFactButton();
    }

    @Если("Нажать ILS Admin")
    @When("Click ILS Admin")
    public void clickILSAdmin() {
        _serviceChoisePage.clickILSAdminButton();
    }

    @Если("Нажать ILS Oper")
    @When("Click ILS Oper")
    public void clickILSOper() {
        _serviceChoisePage.clickILSOperButton();
    }


}
