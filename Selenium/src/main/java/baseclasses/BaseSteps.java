package baseclasses;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.ui.ExpectedConditions;
import utils.WebDriverManager;
import java.util.ArrayList;
import java.util.HashMap;


public class BaseSteps extends WebDriverManager {

    protected HashMap<String, String> _memoryData;


    public void AfterTest() {
        deleteCookies();
        goBack();
        refreshPage();
    }

    // ---------------------------------------- Base Browser Steps ----------------------------------------------------

    public void getUrl(String url) {
        _driver.get(url);
    }

    public String getCurrentUrl() {
        return _driver.getCurrentUrl();
    }

    public String getBrowserWindowTitle() {
        return _driver.getTitle();
    }

    public void maximizeBrowserWindow() {
        _driver.manage().window().maximize();
    }

    public ArrayList<String> getBrowserTabs() {
        return new ArrayList<String> (_driver.getWindowHandles());
    }

    public int getBrowserTabsCount() {
        ArrayList<String> browserTabs = getBrowserTabs();
        return browserTabs.size();
    }

    public void switchToBrowserTab(int tabnumber) {
        ArrayList<String> _tabs = getBrowserTabs();
        _driver.switchTo().window(_tabs.get(tabnumber));
    }

    public boolean isPageBlocked() {
        try {
            if (_driver.findElement(By.id("ils-wait")).getAttribute("style") != null) {
                return !(_driver.findElement(By.id("ils-wait")).getAttribute("style").equals("display: none;"));
            } else {
                return false;
            }
        } catch (Exception e) {
            return false;
        }
    }

    public void waitWhilePageIsBlocked() {
        while (isPageBlocked()) {
            synchronized (_driver) {
                try {
                    _driver.wait(1);
                } catch (Exception e) {
                    System.out.println(e);
                }
            }
        }
    }

    // ----------------------------------------- Navigation ---------------------------------------------------------

    public void refreshPage() {
        _driver.navigate().refresh();
    }

    public void goForward() {
        _driver.navigate().forward();
    }

    public void goBack() {
        _driver.navigate().back();
    }

    // ------------------------------------------ Cookies ----------------------------------------------------------

    public void deleteCookies() {
        _driver.manage().deleteAllCookies();
    }

    // ---------------------------------------- Base WebElements Steps ------------------------------------------------

    public void ClickButton(WebElement button) {
        _waiter.until(ExpectedConditions.elementToBeClickable(button));
        button.click();
    }

    public void ClickLink(WebElement link) {
        _waiter.until(ExpectedConditions.elementToBeClickable(link));
        link.click();
    }

    // ----------------------------------------- Base Assertions --------------------------------------------------

    public static <T> void assertEquals (T ExpectedResult, T ActualResult) {
        Assert.assertEquals("Expected: " + ExpectedResult.toString() + ". Actual: " + ActualResult.toString() + ".", ExpectedResult, ActualResult);
    }

    public static <T> void assertNotEquals (T ExpectedResult, T ActualResult) {
        Assert.assertNotEquals("Expected: not " + ExpectedResult.toString() + ". Actual: " + ActualResult.toString() + ".", ExpectedResult, ActualResult);
    }

}
