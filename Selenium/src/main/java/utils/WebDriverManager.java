package utils;

import org.openqa.selenium.WebDriver;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.firefox.FirefoxDriver;
import org.openqa.selenium.support.ui.WebDriverWait;

import java.util.ArrayList;
import java.util.concurrent.TimeUnit;

public class WebDriverManager {

    protected static long IMPLICIT_WAIT_TIMEOUT = 5;
    protected static WebDriver _driver;
    protected static WebDriverWait _waiter;

    // -------------------------------- Browser ------------------------------------------------------------------

    public void openBrowser(String browser, String driverPath, long timeOut){
        if (_driver == null){
            switch (browser) {
                case "chrome":
                    System.setProperty("webdriver.chrome.driver", driverPath);
                    _driver = new ChromeDriver();
                    break;
                case "firefox":
                    System.setProperty("webdriver.gecko.driver", driverPath);
                    _driver = new FirefoxDriver();
                    break;
                default:
                    throw new IllegalStateException("Browser not support!");
            }
            if (timeOut > 0) {IMPLICIT_WAIT_TIMEOUT = timeOut;}
            _driver.manage().timeouts().implicitlyWait(IMPLICIT_WAIT_TIMEOUT, TimeUnit.SECONDS);
            _waiter = new WebDriverWait(_driver, IMPLICIT_WAIT_TIMEOUT);
        } else {
            throw new IllegalStateException("Driver has already been initialized. Quit it before using this method");
        }
    }

    public void closeBrowser(){
        if (_driver != null){
            _driver.quit();
            _driver = null;
        }
    }

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

    public void switchToBrowserTab(int tabnumber) {
        ArrayList<String> _tabs = getBrowserTabs();
        _driver.switchTo().window(_tabs.get(tabnumber));
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
}
