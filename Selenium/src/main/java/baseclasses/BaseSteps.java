package baseclasses;

import cucumber.api.java.After;
import cucumber.api.java.Before;
import cucumber.api.java.en.Given;
import cucumber.api.java.en.Then;
import cucumber.api.java.en.When;
import cucumber.api.java.ru.����;
import cucumber.api.java.ru.����;
import cucumber.api.java.ru.��;
import org.junit.Assert;
import utils.WebDriverManager;

public class BaseSteps extends WebDriverManager {

    private BasePage _basePage = null;

    // -------------------------------------------- Hooks ---------------------------------------------------------

    @Before
    public void beforeTest(){
        _basePage = new BasePage(_driver);
    }

    @After
    public void afterTest() {
        deleteCookies();
        _basePage = null;
    }

    // --------------------------------------------- Preconditions ------------------------------------------------

    @����("����� = (.*)")
    @Given("URL = (.*)")
    @����("������� (.*)")
    @When("Open (.*)")
    public void openUrl(String url) {
        getUrl(url);
    }

    // --------------------------------------------------- Steps --------------------------------------------------

    @����("������������� �� ������� (.*)")
    @When("Switch to tab (.*)")
    public void switchToTab(int tabnumber) {
        switchToBrowserTab(tabnumber - 1);
    }

    @����("�������� �� ������")
    @When("Click version")
    public void clickVersion() {
        _basePage.clickVersion();
    }

    @����("������ �� �������")
    @When("Click logo")
    public void clickLogo() {
        _basePage.clickLogo();
    }

    @����("������ ���� ������������")
    @When("Click usermenu")
    public void clickUser() {
        _basePage.clickUserMenu();
    }

    @����("������ �����")
    @When("Click Exit")
    public void clickExit() {
        _basePage.clickMenuUserExit();
    }

    @����("������� ���� (.*)")
    @When("Select language (.*)")
    public void selectLanguage(String language) {
         _basePage.selectLanguage(language);
    }

    // ---------------------------------------- Assertions --------------------------------------------------------

    @��("������� URL = (.*)")
    @Then("Current URL = (.*)")
    public void AssertCurrentUrl(String url) {
        Assert.assertEquals(url, getCurrentUrl());
    }

}
