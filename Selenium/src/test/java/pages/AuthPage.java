package pages;

import baseclasses.BasePage;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;


public class AuthPage extends BasePage {

    // ---------------------------------- Page Constructor ------------------------------------------------------

    public AuthPage(WebDriver driver){
        super(driver);
    }

    // ---------------------------------- AuthoriZation WebElements ---------------------------------------------

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

    // -----------------------------Authorization Events -------------------------------------------

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

}
