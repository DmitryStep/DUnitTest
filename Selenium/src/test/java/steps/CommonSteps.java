package steps;

import baseclasses.BasePage;
import baseclasses.BaseSteps;
import cucumber.api.java.After;
import cucumber.api.java.Before;
import cucumber.api.java.en.Given;
import cucumber.api.java.en.Then;
import cucumber.api.java.en.When;
import cucumber.api.java.ru.����;
import cucumber.api.java.ru.����;
import cucumber.api.java.ru.��;

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

    @����("����� = (.*)")
    @Given("URL = (.*)")
    @����("������� (.*)")
    @When("Open (.*)")
    public void openUrl(String url) {
        getUrl(url);
    }

    // ------------------------------------------ Common Steps ------------------------------------------------------

    @����("��������� �������� ��������")
    @When("Wait for page loading")
    public void waitForPageLoading() {
        waitWhilePageIsBlocked();
    }

    @����("�������� ��������")
    @When("Refresh page")
    public void refresh() {
        refreshPage();
    }

    @����("������� ���� �������� �� ���� �����")
    @When("Maximize browser window")
    public void MaximizeBrowserWindow() {
        maximizeBrowserWindow();
    }

    @����("������������� �� ������� (.*)")
    @When("Switch to tab (.*)")
    public void switchToTab(int tabnumber) {
        switchToBrowserTab(tabnumber - 1);
    }

    @����("������ ������ (.*)")
    @When("Click button (.*)")
    public void clickButton(String buttonLabel) {
        ClickButton(_basePage.button(buttonLabel));
    }

    @����("������ ������ (.*)")
    @When("Click link (.*)")
    public void clickLink(String linkLabel) {
        ClickLink(_basePage.link(linkLabel));
    }

    // --------------------------------------- Common Assertions -----------------------------------------------------
    @��("������� URL = (.*)")
    @Then("Current URL = (.*)")
    public void AssertCurrentUrl(String url) {
        assertEquals (url, getCurrentUrl());
    }

    @��("��������� �������� (.*)")
    @Then("Browser title is (.*)")
    public void AssertBrowserTitle(String browserTitle) {
        assertEquals(browserTitle, getBrowserWindowTitle());
    }

    @��("���������� ������� = (.*)")
    @Then("Tabs count = (.*)")
    public void AssertBrowserTabsCount(int tabsCount) {
        assertEquals(tabsCount, getBrowserTabsCount());
    }
}
