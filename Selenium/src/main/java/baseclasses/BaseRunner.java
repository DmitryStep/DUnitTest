package baseclasses;

import configuration.ConfigurationFile;
import configuration.ConfigurationManager;
import utils.WebDriverManager;
import java.io.File;


public class BaseRunner {

    private static ConfigurationManager _configFile;
    protected static WebDriverManager _webDriverManager = null;


    protected static void beforeScenarios(ConfigurationFile configFileAnnotation) {
        _webDriverManager = new WebDriverManager();
        _configFile = new ConfigurationManager();
        String configFileName = "default.properties";
        if (configFileAnnotation != null) {
            configFileName = configFileAnnotation.value();
        }
        File file = new File("src/test/resources/configurations/" + configFileName);
        String _absoluteFilePath = file.getAbsolutePath();
        try {
            _configFile.loadConfig(_absoluteFilePath);
            _webDriverManager.openBrowser(_configFile.browser, _configFile.driverPath, _configFile.timeOut);
        } catch (Exception e) {
            System.err.println("Could not load configuration file!");
            System.exit(1);
        }
    }

    public static void afterScenarios() {
        if (_webDriverManager != null) {
            _webDriverManager.closeBrowser();
            _webDriverManager = null;
        }
    }

}
