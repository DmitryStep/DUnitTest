package baseclasses;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import utils.WebDriverManager;

public class BaseRunner {

    protected static WebDriverManager _webDriverManager = null;

    public static void beforeScenarios(String browser, String driverPath, long timeOut) {
        _webDriverManager = new WebDriverManager();
        _webDriverManager.openBrowser(browser, driverPath, timeOut);
    }

    public static void afterScenarios() {
        _webDriverManager.closeBrowser();
        _webDriverManager = null;
    }

}
