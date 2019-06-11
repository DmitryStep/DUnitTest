package baseclasses;

import org.openqa.selenium.WebDriver;


public class BasePage {

    protected WebDriver _driver;

    public BasePage(WebDriver driver) {
        this._driver = driver;
    }
}
