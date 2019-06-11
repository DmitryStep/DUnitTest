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
        return _driver.findElement(By.xpath(".//*[@id=\"ils-body\"]/div/div/form/input[@class=\"submit\"]"));
    }

    // Поле Логин
    public WebElement loginTextField() {
        return _driver.findElement(By.cssSelector("#ils-auth-login"));
    }

    // Поле Пароль
    public WebElement passwordTextField() {
        return _driver.findElement(By.cssSelector("#ils-auth-password"));
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

}
