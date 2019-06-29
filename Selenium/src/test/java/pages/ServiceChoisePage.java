package pages;

import baseclasses.BasePage;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;

public class ServiceChoisePage extends BasePage {

    public ServiceChoisePage(WebDriver driver) {
        super(driver);
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
        ILSPlanButton().click();
        return this;
    }

    // ������� �� ������ ILSFact
    public ServiceChoisePage clickILSFactButton(){
        ILSFactButton().click();
        return this;
    }

    // ������� �� ������ ILSAdmin
    public ServiceChoisePage clickILSAdminButton(){
        ILSAdminButton().click();
        return this;
    }

    // ������� �� ������ ILSOper
    public ServiceChoisePage clickILSOperButton(){
        ILSOperButton().click();
        return this;
    }

}
