package steps;

import cucumber.api.java.After;
import cucumber.api.java.Before;
import cucumber.api.java.en.Given;
import cucumber.api.java.en.Then;
import cucumber.api.java.en.When;
import cucumber.api.java.ru.����;
import cucumber.api.java.ru.����;
import cucumber.api.java.ru.��;
import org.junit.Assert;
import pages.HeaderPage;
import utils.WebDriverManager;


public class HeaderPageSteps extends WebDriverManager {

    private HeaderPage _headerPage = null;

    // -------------------------------------------- Hooks ---------------------------------------------------------

    @Before
    public void beforeTest(){
        _headerPage = new HeaderPage(_driver);
    }

    @After
    public void afterTest() {
        deleteCookies();
        refreshPage();
        _headerPage = null;
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
        _headerPage.clickVersion();
    }

    @����("������ �� �������")
    @When("Click logo")
    public void clickLogo() {
        _headerPage.clickLogo();
    }

    @����("������ ���� ������������")
    @When("Click usermenu")
    public void clickUser() {
        _headerPage.clickUserMenu();
    }

    @����("������ �����")
    @When("Click Exit")
    public void clickExit() {
        _headerPage.clickMenuUserExit();
    }

    @����("������� ���� (.*)")
    @When("Select language (.*)")
    public void selectLanguage(String language) {
        _headerPage.selectLanguage(language);
    }

    @����("��������� (.*) ���")
    @When("Wait (.*) sec")
    public void waitSec(long timeOut) {
        waitPage(timeOut);
    }

    @����("�������� ��������")
    @When("Refresh page")
    public void refresh() {
        refreshPage();
    }

    // ---------------------------------------- Assertions --------------------------------------------------------

    @��("������� URL = (.*)")
    @Then("Current URL = (.*)")
    public void AssertCurrentUrl(String url) {
        Assert.assertEquals(url, getCurrentUrl());
    }

    @��("�������� ���� (.*)")
    @Then("Active language (.*)")
    public void AssertActiveLanguage(String language) {
        Assert.assertEquals(language, _headerPage.getLanguage());
    }

    @��("��� ������������ = (.*)")
    @Then("Username = (.*)")
    public void AssertActiveUserName(String username){
        Assert.assertEquals(username, _headerPage.menuUser().getText());
    }

    @��("��������� �������� = (.*)")
    @Then("Pagetitle = (.*)")
    public void AssertPageTitle(String ExpectedPageTitle){
        Assert.assertEquals(ExpectedPageTitle, _headerPage.pageTitle());
    }

    @��("��������� �������� ������")
    @Then("Pagetitle is empty")
    public void PageTitleIsEmpty() {
        Assert.assertEquals("", _headerPage.pageTitle());
    }

    @��("��������� �������� �� ������")
    @Then("Pagetitle is not empty")
    public void PageTitleIsNotEmpty() {
        Assert.assertNotEquals("", _headerPage.pageTitle());
    }

}
