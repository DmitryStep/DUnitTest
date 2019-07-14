package pageobjects.pages;

import baseclasses.BasePage;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.WebDriverWait;


public class AuthPage extends BasePage {

    // ---------------------------------- Page Constructor ------------------------------------------------------

    public AuthPage(WebDriver driver, WebDriverWait waiter){
        super(driver, waiter);
    }

    // ---------------------------------- AuthoriZation WebElements ---------------------------------------------

    // Кнопка авторизации
    public WebElement authButton() {
        return _driver.findElement(By.xpath(".//*/input[@class=\"submit\"]"));
    }

    // Поле Логин
    public WebElement loginTextField() {
        return _driver.findElement(By.cssSelector("#ils-auth-login"));
    }

    // Поле Пароль
    public WebElement passwordTextField() {
        return _driver.findElement(By.cssSelector("#ils-auth-password"));
    }

    // Сообщение о неверной авторизации
    public String authErrorMessage() {
        return _driver.findElement(By.xpath(".//*/p[@class=\"message\"]")).getText();
    }

    // -----------------------------Authorization Events -------------------------------------------

    // Ввод текста в поле Логин
    public AuthPage typeLogin(String _login) {
        _waiter.until(ExpectedConditions.elementToBeClickable(loginTextField()));
        loginTextField().clear();
        loginTextField().sendKeys(_login);
        return this;
    }

    // Ввод текста в поле Пароль
    public AuthPage typePassword(String _password) {
        _waiter.until(ExpectedConditions.elementToBeClickable(passwordTextField()));
        passwordTextField().clear();
        passwordTextField().sendKeys(_password);
        return this;
    }

    // Нажатие на кнопку авторизации
    public AuthPage clickAuthButton() {
        _waiter.until(ExpectedConditions.elementToBeClickable(authButton()));
        authButton().click();
        return this;
    }

}
