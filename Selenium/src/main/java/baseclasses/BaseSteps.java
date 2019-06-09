package baseclasses;

import cucumber.api.java.After;
import cucumber.api.java.Before;
import cucumber.api.java.en.Given;
import cucumber.api.java.en.Then;
import cucumber.api.java.en.When;
import cucumber.api.java.ru.Дано;
import cucumber.api.java.ru.Если;
import cucumber.api.java.ru.То;
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
        _basePage.clickVersion();
    }

    @Если("Нажать на логотип")
    @When("Click logo")
    public void clickLogo() {
        _basePage.clickLogo();
    }

    @Если("Нажать меню пользователя")
    @When("Click usermenu")
    public void clickUser() {
        _basePage.clickUserMenu();
    }

    @Если("Нажать Выход")
    @When("Click Exit")
    public void clickExit() {
        _basePage.clickMenuUserExit();
    }

    @Если("Выбрать язык (.*)")
    @When("Select language (.*)")
    public void selectLanguage(String language) {
         _basePage.selectLanguage(language);
    }

    // ---------------------------------------- Assertions --------------------------------------------------------

    @То("Текущая URL = (.*)")
    @Then("Current URL = (.*)")
    public void AssertCurrentUrl(String url) {
        Assert.assertEquals(url, getCurrentUrl());
    }

}
