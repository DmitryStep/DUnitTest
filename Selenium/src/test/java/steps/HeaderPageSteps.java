package steps;

import baseclasses.BaseSteps;
import cucumber.api.java.After;
import cucumber.api.java.Before;
import cucumber.api.java.en.Then;
import cucumber.api.java.en.When;
import cucumber.api.java.ru.Если;
import cucumber.api.java.ru.То;
import pageobjects.pages.HeaderPage;


public class HeaderPageSteps extends BaseSteps {

    private HeaderPage _headerPage = null;

    // -------------------------------------------- Hooks ---------------------------------------------------------

    @Before(order = 2)
    public void beforeTest(){
        _headerPage = new HeaderPage(_driver, _waiter);
    }

    @After(order = 2)
    public void afterTest() {
        AfterTest();
        _headerPage = null;
    }

    // --------------------------------------------------- Steps --------------------------------------------------

    @Если("Кликнуть по версии")
    @When("Click version")
    public void clickVersion() {
        _headerPage.clickVersion();
    }

    @Если("Нажать на логотип")
    @When("Click logo")
    public void clickLogo() {
        _headerPage.clickLogo();
    }

    @Если("Нажать меню пользователя")
    @When("Click usermenu")
    public void clickUser() {
        _headerPage.clickUserMenu();
    }

    @Если("Выбрать пункт меню (.*)")
    @When("Select submenu (.*)")
    public void clickSubMenu(String menuText) {
        _headerPage.clickMenu(menuText);
    }

    @Если("Выбрать язык (.*)")
    @When("Select language (.*)")
    public void SelectLanguage(String language) {
        _headerPage.selectLanguage(language);
    }

    // ---------------------------------------- Assertions --------------------------------------------------------

    @То("Активный язык (.*)")
    @Then("Active language (.*)")
    public void AssertActiveLanguage(String language) {
        assertEquals(language, _headerPage.getLanguage());
    }

    @То("Аккаунт пользователя = (.*)")
    @Then("User account = (.*)")
    public void AssertActiveUserName(String username){
        assertEquals(username, _headerPage.menuUser().getText());
    }

    @То("Заголовок страницы = (.*)")
    @Then("Pagetitle = (.*)")
    public void AssertPageTitle(String ExpectedPageTitle){
        assertEquals(ExpectedPageTitle, _headerPage.pageTitle());
    }

    @То("Заголовок страницы пустой")
    @Then("Pagetitle is empty")
    public void PageTitleIsEmpty() {
        assertEquals("", _headerPage.pageTitle());
    }

    @То("Заголовок страницы не пустой")
    @Then("Pagetitle is not empty")
    public void PageTitleIsNotEmpty() {
        assertNotEquals("", _headerPage.pageTitle());
    }

}
