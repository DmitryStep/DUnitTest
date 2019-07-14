package pageobjects.pages;

import baseclasses.BasePage;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.WebDriverWait;

public class ServiceChoisePage extends BasePage {

    public ServiceChoisePage(WebDriver driver, WebDriverWait waiter) {
        super(driver, waiter);
    }

    // ----------------------------------- ServiceChoisePage WebElements --------------------------------------

    // Кнопка ILSPlan
    public WebElement ILSPlanButton() {
        return _driver.findElement(By.xpath(".//*/a[contains(@href, \"Log.htm\")]"));
    }

    // Кнопка ILSFact
    public WebElement ILSFactButton() {
        return _driver.findElement(By.xpath(".//*/a[contains(@href, \"Mon.htm\")]"));
    }

    // Кнопка ILSAdmin
    public WebElement ILSAdminButton() {
        return _driver.findElement(By.xpath(".//*/a[contains(@href, \"Adm.htm\")]"));
    }

    // Кнопка ILSOper
    public WebElement ILSOperButton() {
        return _driver.findElement(By.xpath(".//*/a[contains(@href, \"Opr.htm\")]"));
    }

    // Надпись над кнопкой ILSPlan
    public String ILSPlanLabel() {
        return _driver.findElement(By.xpath(".//*/a[contains(@href, \"Log.htm\")]/preceding-sibling::h2")).getText();
    }

    // Надпись над кнопкой ILSFact
    public String ILSFactLabel() {
        return _driver.findElement(By.xpath(".//*/a[contains(@href, \"Mon.htm\")]/preceding-sibling::h2")).getText();
    }

    // Надпись над кнопкой ISAdmin
    public String ILSAdminLabel() {
        return _driver.findElement(By.xpath(".//*/a[contains(@href, \"Adm.htm\")]/preceding-sibling::h2")).getText();
    }

    // Надпись над кнопкой ILSOper
    public String ILSOperLabel() {
        return _driver.findElement(By.xpath(".//*/a[contains(@href, \"Opr.htm\")]/preceding-sibling::h2")).getText();
    }

    //-------------------------- ServiceChoisePage Events ---------------------------------------------

    // Нажатие на кнопку ILSPlan
    public ServiceChoisePage clickILSPlanButton() {
        _waiter.until(ExpectedConditions.elementToBeClickable(ILSPlanButton()));
        ILSPlanButton().click();
        return this;
    }

    // Нажатие на кнопку ILSFact
    public ServiceChoisePage clickILSFactButton(){
        _waiter.until(ExpectedConditions.elementToBeClickable(ILSFactButton()));
        ILSFactButton().click();
        return this;
    }

    // Нажатие на кнопку ILSAdmin
    public ServiceChoisePage clickILSAdminButton(){
        _waiter.until(ExpectedConditions.elementToBeClickable(ILSAdminButton()));
        ILSAdminButton().click();
        return this;
    }

    // Нажатие на кнопку ILSOper
    public ServiceChoisePage clickILSOperButton(){
        _waiter.until(ExpectedConditions.elementToBeClickable(ILSOperButton()));
        ILSOperButton().click();
        return this;
    }

}
