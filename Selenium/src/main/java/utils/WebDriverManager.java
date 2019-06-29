package utils;

import org.junit.Assert;
import org.junit.ComparisonFailure;
import org.openqa.selenium.Alert;
import org.openqa.selenium.By;
import org.openqa.selenium.NoAlertPresentException;
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
            _driver.manage().timeouts().implicitlyWait(30, TimeUnit.SECONDS);
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



}
