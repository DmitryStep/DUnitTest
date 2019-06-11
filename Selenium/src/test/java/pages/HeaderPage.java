package pages;

import baseclasses.BasePage;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.ui.Select;

import java.util.List;

public class HeaderPage extends BasePage {

    public HeaderPage(WebDriver driver) {
        super(driver);
    }


    // -------------------------------------- Page Objects ----------------------------------------------

    // ��������� ��������
    public String pageTitle() {
        return _driver.findElement(By.xpath("/html/body/div[2]/div[2]/h1")).getText();
    }

    // �������
    public WebElement Logo() {
        return _driver.findElement(By.xpath("/html/body/div[2]/div[1]/a/img"));
    }

    // �������������� ���� ������������
    public WebElement menuUser() {
        return _driver.findElement(By.id("dropdownMenuLink"));
    }


    // ������ ���� ������������
    public List<WebElement> getMenuUserItems() {
        return _driver.findElements(By.xpath("/html/body/div[3]/div[5]/div/a[@class=\"dropdown-item\"]"));
    }

    // ����� ���� "�����"
    public WebElement menuUserExit() {
        return _driver.findElement(By.xpath(".//*[@onclick=\"$.ils.logout()\"]"));
    }

    // ����� ���� "������������� �������"
    public WebElement menuUserProfile() {
        return _driver.findElement(By.xpath(".//*[@onclick=\"$.ils.editProfile()\"]"));
    }

    // �������������� ������ ������ �����
    public WebElement menuLanguage() {
        return _driver.findElement(By.id("lang"));
    }

    // ������ "������"
    public WebElement versionLink() {
        return _driver.findElement(By.xpath(".//*/html/body/div[3]/div[6]/div"));
    }


    // -------------------------------------------- Page Events ------------------------------------------------

    //���� �� ��������
    public void clickLogo() {
        Logo().click();
    }

    // ��������� ��������� ���� ������������
    public boolean isUserMenuVisible() {
        return (_driver.findElement(By.xpath("/html/body/div[2]/div[@class=\"user dropdown show\"")) != null);
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
        versionLink().click();
    }

    // ���� �� ����������������� ����
    public void clickUserMenu(){
        menuUser().click();
    }

    // ���� �� ���� �����
    public void clickMenuUserExit(){
        menuUserExit().click();
    }

}
