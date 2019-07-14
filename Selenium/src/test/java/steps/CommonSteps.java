package steps;

import baseclasses.BasePage;
import baseclasses.BaseSteps;
import cucumber.api.java.After;
import cucumber.api.java.Before;
import cucumber.api.java.en.Given;
import cucumber.api.java.en.Then;
import cucumber.api.java.en.When;
import cucumber.api.java.ru.Дано;
import cucumber.api.java.ru.Если;
import cucumber.api.java.ru.То;

import java.util.ArrayList;
import java.util.List;

public class CommonSteps extends BaseSteps {

    private BasePage _basePage = null;

    // -------------------------------------------- Hooks ---------------------------------------------------------

    @Before(order = 0)
    public void beforeTest(){
        _basePage = new BasePage(_driver, _waiter);
    }

    @After(order = 0)
    public void afterTest() {
        AfterTest();
        _basePage = null;
    }

    // ------------------------------------------ Common Preconditions -----------------------------------------------

    @Дано("Адрес = (.*)")
    @Given("URL = (.*)")
    @Если("Открыть (.*)")
    @When("Open (.*)")
    public void openUrl(String url) {
        getUrl(url);
    }

    // ------------------------------------------ Common Steps ------------------------------------------------------

    @Если("Дождаться загрузки страницы")
    @When("Wait for page loading")
    public void waitForPageLoading() {
        waitWhilePageIsBlocked();
    }

    @Если("Обновить страницу")
    @When("Refresh page")
    public void refresh() {
        refreshPage();
    }

    @Если("Открыть окно браузера на весь экран")
    @When("Maximize browser window")
    public void MaximizeBrowserWindow() {
        maximizeBrowserWindow();
    }

    @Если("Переключиться на вкладку (.*)")
    @When("Switch to tab (.*)")
    public void switchToTab(int tabnumber) {
        switchToBrowserTab(tabnumber - 1);
    }

    @Если("Нажать кнопку (.*)")
    @When("Click button (.*)")
    public void clickButton(String buttonLabel) {
        ClickButton(_basePage.button(buttonLabel));
    }

    @Если("Нажать ссылку (.*)")
    @When("Click link (.*)")
    public void clickLink(String linkLabel) {
        ClickLink(_basePage.link(linkLabel));
    }

    // --------------------------------------- Common Assertions -----------------------------------------------------
    @То("Текущая URL = (.*)")
    @Then("Current URL = (.*)")
    public void AssertCurrentUrl(String url) {
        assertEquals (url, getCurrentUrl());
    }

    @То("Заголовок браузера (.*)")
    @Then("Browser title is (.*)")
    public void AssertBrowserTitle(String browserTitle) {
        assertEquals(browserTitle, getBrowserWindowTitle());
    }

    @То("Количество вкладок = (.*)")
    @Then("Tabs count = (.*)")
    public void AssertBrowserTabsCount(int tabsCount) {
        assertEquals(tabsCount, getBrowserTabsCount());
    }
}
