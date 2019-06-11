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

    // Заголовок страницы
    public String pageTitle() {
        return _driver.findElement(By.xpath("/html/body/div[2]/div[2]/h1")).getText();
    }

    // Логотип
    public WebElement Logo() {
        return _driver.findElement(By.xpath("/html/body/div[2]/div[1]/a/img"));
    }

    // Раскрывающееся меню пользователя
    public WebElement menuUser() {
        return _driver.findElement(By.id("dropdownMenuLink"));
    }


    // Пункты меню пользователя
    public List<WebElement> getMenuUserItems() {
        return _driver.findElements(By.xpath("/html/body/div[3]/div[5]/div/a[@class=\"dropdown-item\"]"));
    }

    // Пункт меню "Выход"
    public WebElement menuUserExit() {
        return _driver.findElement(By.xpath(".//*[@onclick=\"$.ils.logout()\"]"));
    }

    // Пункт меню "Редактировать профиль"
    public WebElement menuUserProfile() {
        return _driver.findElement(By.xpath(".//*[@onclick=\"$.ils.editProfile()\"]"));
    }

    // Раскрывающийся список выбора языка
    public WebElement menuLanguage() {
        return _driver.findElement(By.id("lang"));
    }

    // Ссылка "Версия"
    public WebElement versionLink() {
        return _driver.findElement(By.xpath(".//*/html/body/div[3]/div[6]/div"));
    }


    // -------------------------------------------- Page Events ------------------------------------------------

    //Клик по логотипу
    public void clickLogo() {
        Logo().click();
    }

    // Проверяем видимость меню пользователя
    public boolean isUserMenuVisible() {
        return (_driver.findElement(By.xpath("/html/body/div[2]/div[@class=\"user dropdown show\"")) != null);
    }

    // Выбор языка
    public void selectLanguage(String _language) {
        new Select(menuLanguage()).selectByVisibleText(_language);
    }

    // Получение активного языка
    public String getLanguage() {
        return new Select(menuLanguage()).getFirstSelectedOption().getText();
    }

    // Клик по ссылке "Версия"
    public void clickVersion(){
        versionLink().click();
    }

    // Кдик по пользовательскому меню
    public void clickUserMenu(){
        menuUser().click();
    }

    // Клик по меню Выход
    public void clickMenuUserExit(){
        menuUserExit().click();
    }

}
