package pageobjects.pages;

import baseclasses.BasePage;
import org.junit.Assert;
import org.openqa.selenium.*;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.Select;
import org.openqa.selenium.support.ui.WebDriverWait;


public class HeaderPage extends BasePage {

    public HeaderPage(WebDriver driver, WebDriverWait waiter) {
        super(driver, waiter);
    }

    // -------------------------------------- HeaderPage WebElements ----------------------------------------------

    // ��������� ��������
    public String pageTitle() {
        return _driver.findElement(By.xpath(".//*[@class=\"title\"]/h1")).getText();
    }

    // �������
    public WebElement Logo() {
        return _driver.findElement(By.xpath(".//*[@class=\"logo\"]/a/img"));
    }

    // �������������� ���� ������������
    public WebElement menuUser() {
        return _driver.findElement(By.id("dropdownMenuLink"));
    }

    // �������������� ������ ������ �����
    public WebElement menuLanguage() {
        return _driver.findElement(By.id("lang"));
    }

    // ������ "������"
    public WebElement versionLink() {
        return _driver.findElement(By.className("version"));
    }

    // ������ "������ �� ���������"
    public WebElement wrongVersionLink() {
        return _driver.findElement(By.xpath(".//*[@class=\"wrong-version\"]"));
    }


    // -------------------------------------------- HeaderPage Events ------------------------------------------------

    //���� �� ��������
    public void clickLogo() {
        _waiter.until(ExpectedConditions.elementToBeClickable(Logo()));
        Logo().click();
    }

    // ����� �����
    public void selectLanguage(String _language) {
        _waiter.until(ExpectedConditions.elementToBeClickable(menuLanguage()));
        new Select(menuLanguage()).selectByVisibleText(_language);
    }

    // ��������� ��������� �����
    public String getLanguage() {
        return new Select(menuLanguage()).getFirstSelectedOption().getText();
    }

    // ���� �� ������ "������"
    public void clickVersion(){
        try {
            _waiter.until(ExpectedConditions.elementToBeClickable(wrongVersionLink()));
            wrongVersionLink().click();
        } catch (Exception e) {
            _waiter.until(ExpectedConditions.elementToBeClickable(versionLink()));
            versionLink().click();
        }
    }

    // ���� �� ����������������� ����
    public void clickUserMenu(){
        _waiter.until(ExpectedConditions.elementToBeClickable(menuUser()));
        if (menuUser().isEnabled()) {
            menuUser().click();
        }
    }

    // ���� �� ������ ����
    public void clickMenu(String menuText){
        if (_driver.findElement(By.xpath(".//*[text()=\"" + menuText +"\"]")).isEnabled()) {
            try {
                _waiter.until(ExpectedConditions.elementToBeClickable(_driver.findElement(By.xpath(".//*[text()=\"" + menuText + "\"]"))));
                _driver.findElement(By.xpath(".//*[text()=\"" + menuText + "\"]")).click();
            } catch (Exception e) {
                Assert.fail("SubMenu " + menuText + " not visible or disabled!");
            }
        }
    }

}
