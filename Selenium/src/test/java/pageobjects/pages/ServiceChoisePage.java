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

    // ������ ILSPlan
    public WebElement ILSPlanButton() {
        return _driver.findElement(By.xpath(".//*/a[contains(@href, \"Log.htm\")]"));
    }

    // ������ ILSFact
    public WebElement ILSFactButton() {
        return _driver.findElement(By.xpath(".//*/a[contains(@href, \"Mon.htm\")]"));
    }

    // ������ ILSAdmin
    public WebElement ILSAdminButton() {
        return _driver.findElement(By.xpath(".//*/a[contains(@href, \"Adm.htm\")]"));
    }

    // ������ ILSOper
    public WebElement ILSOperButton() {
        return _driver.findElement(By.xpath(".//*/a[contains(@href, \"Opr.htm\")]"));
    }

    // ������� ��� ������� ILSPlan
    public String ILSPlanLabel() {
        return _driver.findElement(By.xpath(".//*/a[contains(@href, \"Log.htm\")]/preceding-sibling::h2")).getText();
    }

    // ������� ��� ������� ILSFact
    public String ILSFactLabel() {
        return _driver.findElement(By.xpath(".//*/a[contains(@href, \"Mon.htm\")]/preceding-sibling::h2")).getText();
    }

    // ������� ��� ������� ISAdmin
    public String ILSAdminLabel() {
        return _driver.findElement(By.xpath(".//*/a[contains(@href, \"Adm.htm\")]/preceding-sibling::h2")).getText();
    }

    // ������� ��� ������� ILSOper
    public String ILSOperLabel() {
        return _driver.findElement(By.xpath(".//*/a[contains(@href, \"Opr.htm\")]/preceding-sibling::h2")).getText();
    }

    //-------------------------- ServiceChoisePage Events ---------------------------------------------

    // ������� �� ������ ILSPlan
    public ServiceChoisePage clickILSPlanButton() {
        _waiter.until(ExpectedConditions.elementToBeClickable(ILSPlanButton()));
        ILSPlanButton().click();
        return this;
    }

    // ������� �� ������ ILSFact
    public ServiceChoisePage clickILSFactButton(){
        _waiter.until(ExpectedConditions.elementToBeClickable(ILSFactButton()));
        ILSFactButton().click();
        return this;
    }

    // ������� �� ������ ILSAdmin
    public ServiceChoisePage clickILSAdminButton(){
        _waiter.until(ExpectedConditions.elementToBeClickable(ILSAdminButton()));
        ILSAdminButton().click();
        return this;
    }

    // ������� �� ������ ILSOper
    public ServiceChoisePage clickILSOperButton(){
        _waiter.until(ExpectedConditions.elementToBeClickable(ILSOperButton()));
        ILSOperButton().click();
        return this;
    }

}
