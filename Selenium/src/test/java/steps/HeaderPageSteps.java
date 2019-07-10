package steps;

import baseclasses.BaseSteps;
import cucumber.api.java.After;
import cucumber.api.java.Before;
import cucumber.api.java.en.Then;
import cucumber.api.java.en.When;
import cucumber.api.java.ru.����;
import cucumber.api.java.ru.��;
import org.openqa.selenium.By;
import org.openqa.selenium.support.ui.ExpectedConditions;
import pageobjects.pages.HeaderPage;


public class HeaderPageSteps extends BaseSteps {

    private HeaderPage _headerPage = null;

    // -------------------------------------------- Hooks ---------------------------------------------------------

    @Before(order = 2)
    public void beforeTest(){
        _headerPage = new HeaderPage(_driver);
    }

    @After(order = 2)
    public void afterTest() {
        AfterTest();
        _headerPage = null;
    }

    // --------------------------------------------------- Steps --------------------------------------------------

    @����("�������� �� ������")
    @When("Click version")
    public void clickVersion() {
        _waiter.until(ExpectedConditions.elementToBeClickable(_headerPage.versionLink()));
        _headerPage.clickVersion();
    }

    @����("������ �� �������")
    @When("Click logo")
    public void clickLogo() {
        _waiter.until(ExpectedConditions.elementToBeClickable(_headerPage.Logo()));
        _headerPage.clickLogo();
    }

    @����("������ ���� ������������")
    @When("Click usermenu")
    public void clickUser() {
        _waiter.until(ExpectedConditions.elementToBeClickable(_headerPage.menuUser()));
        _headerPage.clickUserMenu();
    }

    @����("������� ����� ���� (.*)")
    @When("Select submenu (.*)")
    public void clickSubMenu(String menuText) {
        _waiter.until(ExpectedConditions.elementToBeClickable(_driver.findElement(By.xpath(".//*[text()=\"" + menuText + "\"]"))));
        _headerPage.clickMenu(menuText);
    }

    @����("������� ���� (.*)")
    @When("Select language (.*)")
    public void SelectLanguage(String language) {
        _waiter.until(ExpectedConditions.elementToBeClickable(_headerPage.menuLanguage()));
        _headerPage.selectLanguage(language);
    }

    // ---------------------------------------- Assertions --------------------------------------------------------

    @��("�������� ���� (.*)")
    @Then("Active language (.*)")
    public void AssertActiveLanguage(String language) {
        assertEquals(language, _headerPage.getLanguage());
    }

    @��("������� ������������ = (.*)")
    @Then("User account = (.*)")
    public void AssertActiveUserName(String username){
        assertEquals(username, _headerPage.menuUser().getText());
    }

    @��("��������� �������� = (.*)")
    @Then("Pagetitle = (.*)")
    public void AssertPageTitle(String ExpectedPageTitle){
        assertEquals(ExpectedPageTitle, _headerPage.pageTitle());
    }

    @��("��������� �������� ������")
    @Then("Pagetitle is empty")
    public void PageTitleIsEmpty() {
        assertEquals("", _headerPage.pageTitle());
    }

    @��("��������� �������� �� ������")
    @Then("Pagetitle is not empty")
    public void PageTitleIsNotEmpty() {
        assertNotEquals("", _headerPage.pageTitle());
    }

}
