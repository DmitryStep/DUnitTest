package steps;

import cucumber.api.java.After;
import cucumber.api.java.Before;
import cucumber.api.java.en.Then;
import cucumber.api.java.en.When;
import cucumber.api.java.ru.Если;
import cucumber.api.java.ru.То;
import org.junit.Assert;
import org.openqa.selenium.support.ui.ExpectedConditions;
import pages.ServiceChoisePage;
import utils.WebDriverManager;

public class ServiceChoisePageSteps extends WebDriverManager {

    private ServiceChoisePage _serviceChoisePage = null;

    //----------------------------------------- Hooks -----------------------------------------------------

    @Before
    public void beforeTest(){
        _serviceChoisePage = new ServiceChoisePage(_driver);
    }

    @After
    public void afterTest() {
        deleteCookies();
        refreshPage();
        _serviceChoisePage = null;
    }

    //----------------------------------------- Steps -----------------------------------------------------


    @Если("Нажать ILS Plan")
    @When("Click ILS Plan")
    public ServiceChoisePage clickILSPlan() {
        _waiter.until(ExpectedConditions.elementToBeClickable(_serviceChoisePage.ILSPlanButton()));
        return _serviceChoisePage.clickILSPlanButton();
    }

    @Если("Нажать ILS Fact")
    @When("Click ILS Fact")
    public ServiceChoisePage clickILSFact() {
        _waiter.until(ExpectedConditions.elementToBeClickable(_serviceChoisePage.ILSFactButton()));
        return _serviceChoisePage.clickILSFactButton();
    }

    @Если("Нажать ILS Admin")
    @When("Click ILS Admin")
    public ServiceChoisePage clickILSAdmin() {
        _waiter.until(ExpectedConditions.elementToBeClickable(_serviceChoisePage.ILSAdminButton()));
        return _serviceChoisePage.clickILSAdminButton();
    }

    @Если("Нажать ILS Oper")
    @When("Click ILS Oper")
    public ServiceChoisePage clickILSOper() {
        _waiter.until(ExpectedConditions.elementToBeClickable(_serviceChoisePage.ILSOperButton()));
        return _serviceChoisePage.clickILSOperButton();
    }

    //--------------------------------------- Assertions --------------------------------------------------

    @То("Подпись для ILS Plan = (.*)")
    @Then("Label for ILS Plan = (.*)")
    public void AssertILSPlanLabel(String labelText) {
        Assert.assertEquals(labelText, _serviceChoisePage.ILSPlanLabel());
    }

    @То("Подпись для ILS Fact = (.*)")
    @Then("Label for ILS Fact = (.*)")
    public void AssertILSFactLabel(String labelText) {
        Assert.assertEquals(labelText, _serviceChoisePage.ILSFactLabel());
    }

    @То("Подпись для ILS Admin = (.*)")
    @Then("Label for ILS Admin = (.*)")
    public void AssertILSAdminLabel(String labelText) {
        Assert.assertEquals(labelText, _serviceChoisePage.ILSAdminLabel());
    }

    @То("Подпись для ILS Oper = (.*)")
    @Then("Label for ILS Oper = (.*)")
    public void AssertILSOperLabel(String labelText) {
        Assert.assertEquals(labelText, _serviceChoisePage.ILSOperLabel());
    }

}
