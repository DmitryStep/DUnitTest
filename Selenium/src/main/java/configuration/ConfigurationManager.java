package configuration;

import java.io.FileInputStream;
import java.io.IOException;
import java.util.Properties;


public class ConfigurationManager {

    public String browser;
    public String driverPath;
    public long timeOut;
    public String baseURL;
    public String baseLogin;
    public String basePassword;


    public void loadConfig(String configFile) {

        FileInputStream _configStream;
        Properties _property;

        _property = new Properties();

        try {
            _configStream = new FileInputStream(configFile);
            _property.load(_configStream);

             browser = _property.getProperty("BROWSER");
             driverPath = _property.getProperty("DRIVER_PATH");
             timeOut = Integer.parseInt(_property.getProperty("TIMEOUT"));
             baseURL = _property.getProperty("BASEURL");
             baseLogin = _property.getProperty("BASELOGIN");
             basePassword = _property.getProperty("BASEPASSWORD");

             _configStream = null;
             _property = null;
        } catch (IOException e) {
            System.err.println("Properties file not found!");
            System.exit(1);
        }
    }

}
