package steps;

import cucumber.api.java.After;
import cucumber.api.java.Before;
import cucumber.api.java.en.Given;
import cucumber.api.java.en.Then;
import cucumber.api.java.en.When;
import cucumber.api.java.ru.Дано;
import cucumber.api.java.ru.Если;
import cucumber.api.java.ru.То;
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

    @Дано("Адрес = (.*)")
    @Given("URL = (.*)")
    @Если("Открыть (.*)")
    @When("Open (.*)")
    public void openUrl(String url) {
        getUrl(url);
    }

    // --------------------------------------------------- Steps --------------------------------------------------

    @Если("Переключиться на вкладку (.*)")
    @When("Switch to tab (.*)")
    public void switchToTab(int tabnumber) {
        switchToBrowserTab(tabnumber - 1);
    }

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

    @Если("Нажать Выход")
    @When("Click Exit")
    public void clickExit() {
        _headerPage.clickMenuUserExit();
    }

    @Если("Выбрать язык (.*)")
    @When("Select language (.*)")
    public void selectLanguage(String language) {
        _headerPage.selectLanguage(language);
    }

    @Если("Подождать (.*) сек")
    @When("Wait (.*) sec")
    public void waitSec(long timeOut) {
        waitPage(timeOut);
    }

    @Если("Обновить страницу")
    @When("Refresh page")
    public void refresh() {
        refreshPage();
    }

    // ---------------------------------------- Assertions --------------------------------------------------------

    @То("Текущая URL = (.*)")
    @Then("Current URL = (.*)")
    public void AssertCurrentUrl(String url) {
        Assert.assertEquals(url, getCurrentUrl());
    }

    @То("Активный язык (.*)")
    @Then("Active language (.*)")
    public void AssertActiveLanguage(String language) {
        Assert.assertEquals(language, _headerPage.getLanguage());
    }

    @То("Имя пользователя = (.*)")
    @Then("Username = (.*)")
    public void AssertActiveUserName(String username){
        Assert.assertEquals(username, _headerPage.menuUser().getText());
    }

    @То("Заголовок страницы = (.*)")
    @Then("Pagetitle = (.*)")
    public void AssertPageTitle(String ExpectedPageTitle){
        Assert.assertEquals(ExpectedPageTitle, _headerPage.pageTitle());
    }

    @То("Заголовок страницы пустой")
    @Then("Pagetitle is empty")
    public void PageTitleIsEmpty() {
        Assert.assertEquals("", _headerPage.pageTitle());
    }

    @То("Заголовок страницы не пустой")
    @Then("Pagetitle is not empty")
    public void PageTitleIsNotEmpty() {
        Assert.assertNotEquals("", _headerPage.pageTitle());
    }

}
