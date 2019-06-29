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

    // Заголовок страницы
    public String pageTitle() {
        return _driver.findElement(By.xpath(".//*[@class=\"title\"]/h1")).getText();
    }

    // Логотип
    public WebElement Logo() {
        return _driver.findElement(By.xpath(".//*[@class=\"logo\"]/a/img"));
    }

    // Раскрывающееся меню пользователя
    public WebElement menuUser() {
        return _driver.findElement(By.id("dropdownMenuLink"));
    }

    // Раскрывающийся список выбора языка
    public WebElement menuLanguage() {
        return _driver.findElement(By.id("lang"));
    }

    // Ссылка "Версия"
    public WebElement versionLink() {
        return _driver.findElement(By.className("version"));
    }

    // Ссылка "Версии не совпадают"
    public WebElement wrongVersionLink() {
        return _driver.findElement(By.xpath(".//*[@class=\"wrong-version\"]"));
    }


    // -------------------------------------------- HeaderPage Events ------------------------------------------------

    //Клик по логотипу
    public void clickLogo() {
        Logo().click();
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
        try {
            wrongVersionLink().click();
        } catch (Exception e) {
            versionLink().click();
        }
    }

    // Клик по пользовательскому меню
    public void clickUserMenu(){
        if (menuUser().isEnabled()) {
            menuUser().click();
        }
    }

    // Клик по пункту меню
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
