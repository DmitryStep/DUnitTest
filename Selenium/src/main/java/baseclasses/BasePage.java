package baseclasses;

import org.openqa.selenium.*;
import org.openqa.selenium.support.ui.WebDriverWait;


public class BasePage {

    protected WebDriver _driver;
    protected WebDriverWait _waiter;

    public BasePage(WebDriver driver, WebDriverWait waiter) {
        this._driver = driver;
        this._waiter = waiter;
    }

    public WebElement button(String buttonLabel) {
        return _driver.findElement(By.xpath(".//*[normalize-space(text())=\"" + buttonLabel + "\"]"));
    }

    public WebElement link(String linkLabel) {
        return _driver.findElement(By.xpath(".//*/a[normalize-space(text())=\"" + linkLabel + "\"]"));
    }

    public boolean isAlertPresent() {
        try {
            _driver.switchTo().alert();
            return true;
        } catch (NoAlertPresentException e) {
            return false;
        }
    }

    public void acceptAlert() {
        try {
            Alert alert = _driver.switchTo().alert();
            String alertText = alert.getText();
            alert.accept();
        } catch (Exception e) {
            System.out.println("Проблемы с алертом!");
        }

    }

    public void dismissAlert() {
        try {
            Alert alert = _driver.switchTo().alert();
            alert.dismiss();
        } catch (Exception e) {
            System.out.println("Проблемы с алертом!");
        }
    }

    public String closeAlertAndGetItsText(boolean acceptNextAlert) {
        try {
            Alert alert = _driver.switchTo().alert();
            String alertText = alert.getText();
            if (acceptNextAlert) {
                alert.accept();
            } else {
                alert.dismiss();
            }
            return alertText;
        } catch (Exception e) {
            System.out.println("Проблемы с алертом!");
            return null;
        }
    }

}
