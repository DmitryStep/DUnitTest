package pages;

import baseclasses.BasePage;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.ui.Select;
import java.util.List;

public class AuthPage extends BasePage {

    // ---------------------------------- Page Constructor ------------------------------------------------------

    public AuthPage(WebDriver driver){
        super(driver);
    }

    // ---------------------------------- AuthoriZation WebElements ---------------------------------------------

    // Заголовок страницы
    public String pageTitle() {
        return _driver.findElement(By.xpath("/html/body/div[2]/div[2]/h1")).getText();
    }

    // Логотип
    public WebElement Logo() {
        return _driver.findElement(By.xpath("/html/body/div[2]/div[1]/a/img"));
    }

    // Кнопка авторизации
    public WebElement authButton() {
        return _driver.findElement(By.xpath(".//*[@id=\"ils-body\"]/div/div/form/input"));
    }

    // Поле Логин
    public WebElement loginTextField() {
        return _driver.findElement(By.id("ils-auth-login"));
    }

    // Поле Пароль
    public WebElement passwordTextField() {
        return _driver.findElement(By.id("ils-auth-password"));
    }

    // Подпись к полю Логин
    public String loginLabel() { return _driver.findElement((By.xpath(".//*[@id=\"ils-body\"]/div/div/form/table/tbody/tr[1]/td[1]"))).getText(); }

    // Подпись к полю Пароль
    public String passwordLabel() { return _driver.findElement((By.xpath(".//*[@id=\"ils-body\"]/div/div/form/table/tbody/tr[2]/td[1]"))).getText(); }

    // Подпись на кнопке Авторизации
    public String authButtonLabel() { return authButton().getText(); }

    // Сообщение о неверной авторизации
    public String authErrorMessage() {
        return _driver.findElement(By.xpath(".//*/div[@id='ils-body']/div/div/form/p")).getText();
    }

    // ----------------------------------- Service Choise WebElements --------------------------------------

    // Раскрывающееся меню пользователя
    public WebElement menuUser() {
        return _driver.findElement(By.id("dropdownMenuLink"));
    }

    // Пункты меню пользователя
    public List<WebElement> menuUserItems() {
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

    // Ссылка "Версия"
    public WebElement versionLink() {
        return _driver.findElement(By.xpath(".//*/html/body/div[3]/div[6]/div"));
    }

    // -----------------------------Authorization Events -------------------------------------------

    //Клик по логотипу
    public AuthPage clickLogo() {
        Logo().click();
        return this;
    }

    // Ввод текста в поле Логин
    public AuthPage typeLogin(String _login) {
        loginTextField().sendKeys(_login);
        return this;
    }

    // Ввод текста в поле Пароль
    public AuthPage typePassword(String _password) {
        passwordTextField().sendKeys(_password);
        return this;
    }

    // Нажатие на кнопку авторизации
    public AuthPage clickAuthButton() {
        authButton().click();
        return this;
    }

    //-------------------------- Service Choise Events ---------------------------------------------

    // Выбор языка
    public AuthPage selectLanguage(String _language) {
        new Select(menuLanguage()).selectByVisibleText(_language);
        return this;
    }

    //
    public String getLanguage() {
        return new Select(menuLanguage()).getFirstSelectedOption().getText();
    }

    // Нажатие на кнопку ILSPlan
    public AuthPage clickILSPlanButton() {
        ILSPlanButton().click();
        return this;
    }

    // Нажатие на кнопку ILSFact
    public AuthPage clickILSFactButton(){
        ILSFactButton().click();
        return this;
    }

    // Нажатие на кнопку ILSAdmin
    public AuthPage clickILSAdminButton(){
        ILSAdminButton().click();
        return this;
    }

    // Нажатие на кнопку ILSOper
    public AuthPage clickILSOperButton(){
        ILSOperButton().click();
        return this;
    }

    // Клик по ссылке "Версия"
    public AuthPage clickVersion(){
        versionLink().click();
        return this;
    }

    // Кдик по пользовательскому меню
    public AuthPage clickUserMenu(){
        menuUser().click();
        return this;
    }

    // Клик по меню Выход
    public AuthPage clickMenuUserExit(){
        menuUserExit().click();
        return this;
    }

}
