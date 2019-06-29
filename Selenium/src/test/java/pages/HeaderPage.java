package pages;

import baseclasses.BasePage;
import net.bytebuddy.implementation.bytecode.Throw;
import org.junit.Assert;
import org.openqa.selenium.*;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.Select;

import java.util.List;

import static java.lang.Thread.sleep;

public class HeaderPage extends BasePage {

    public HeaderPage(WebDriver driver) {
        super(driver);
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
        Logo().click();
    }

    // ����� �����
    public void selectLanguage(String _language) {
        new Select(menuLanguage()).selectByVisibleText(_language);
    }

    // ��������� ��������� �����
    public String getLanguage() {
        return new Select(menuLanguage()).getFirstSelectedOption().getText();
    }

    // ���� �� ������ "������"
    public void clickVersion(){
        try {
            wrongVersionLink().click();
        } catch (Exception e) {
            versionLink().click();
        }
    }

    // ���� �� ����������������� ����
    public void clickUserMenu(){
        if (menuUser().isEnabled()) {
            menuUser().click();
        }
    }

    // ���� �� ������ ����
    public void clickMenu(String menuText){
        if (_driver.findElement(By.xpath(".//*[text()=\"" + menuText +"\"]")).isEnabled()) {
            try {
                _driver.findElement(By.xpath(".//*[text()=\"" + menuText + "\"]")).click();
            } catch (Exception e) {
                Assert.fail("SubMenu " + menuText + " not visible or disabled!");
            }
        }
    }

}
