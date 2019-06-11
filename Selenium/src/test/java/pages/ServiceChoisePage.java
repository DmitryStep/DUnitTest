package pages;

import baseclasses.BasePage;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;

public class ServiceChoisePage extends BasePage {

    public ServiceChoisePage(WebDriver driver) {
        super(driver);
    }

    // ----------------------------------- Service Choise WebElements --------------------------------------

    // Кнопка ILSPlan
    public WebElement ILSPlanButton() {
        return _driver.findElement(By.xpath(".//*[@id='ils-body']/div/div/div[1]/a"));
    }

    // Кнопка ILSFact
    public WebElement ILSFactButton() {
        return _driver.findElement(By.xpath(".//*[@id='ils-body']/div/div/div[2]/a"));
    }

    // Кнопка ILSAdmin
    public WebElement ILSAdminButton() {
        return _driver.findElement(By.xpath(".//*[@id='ils-body']/div/div/div[3]/a"));
    }

    // Кнопка ILSOper
    public WebElement ILSOperButton() {
        return _driver.findElement(By.xpath(".//*[@id='ils-body']/div/div/div[4]/a"));
    }

    // Надпись над кнопкой ILSPlan
    public String ILSPlanLabel() {
        return _driver.findElement(By.xpath(".//*[@id=\"ils-body\"]/div/div/div[1]/h2")).getText();
    }

    // Надпись над кнопкой ILSFact
    public String ILSFactLabel() {
        return _driver.findElement(By.xpath(".//*[@id=\"ils-body\"]/div/div/div[2]/h2")).getText();
    }

    // Надпись над кнопкой ISAdmin
    public String ILSAdminLabel() {
        return _driver.findElement(By.xpath(".//*[@id=\"ils-body\"]/div/div/div[3]/h2")).getText();
    }

    // Надпись над кнопкой ILSOper
    public String ILSOperLabel() {
        return _driver.findElement(By.xpath(".//*[@id=\"ils-body\"]/div/div/div[4]/h2")).getText();
    }

    //-------------------------- Service Choise Events ---------------------------------------------

    // Нажатие на кнопку ILSPlan
    public ServiceChoisePage clickILSPlanButton() {
        ILSPlanButton().click();
        return this;
    }

    // Нажатие на кнопку ILSFact
    public ServiceChoisePage clickILSFactButton(){
        ILSFactButton().click();
        return this;
    }

    // Нажатие на кнопку ILSAdmin
    public ServiceChoisePage clickILSAdminButton(){
        ILSAdminButton().click();
        return this;
    }

    // Нажатие на кнопку ILSOper
    public ServiceChoisePage clickILSOperButton(){
        ILSOperButton().click();
        return this;
    }

}
