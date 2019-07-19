package pageobjects.forms;

import baseclasses.BasePage;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.WebDriverWait;

public class UserProfileForm extends BasePage {

    public UserProfileForm(WebDriver driver, WebDriverWait waiter) {
        super(driver, waiter);
    }

    // ------------------------------------------- UserProfileForm WebElements -------------------------------------

    // Надпись на заголовке
    public String userProfileFormHeaderLabel() {
        return _driver.findElement(By.id("ui-id-3")).getText();
    }

    // Поле "Имя пользователя"
    public WebElement accountNameField() {
        return _driver.findElement(By.id("accName"));
    }

    // Значение поля "Имя пользователя"
    public String userFieldValue() {
        return accountNameField().getAttribute("value").toString();
    }

    // Кнопка "Сохранить"
    public WebElement saveButton() {
        return _driver.findElement(By.xpath(".//*[@onclick=\"$.ils.renameAccount()\"]"));
    }

    // Поле "Старый пароль"
    public WebElement oldPasswordField() {
        return _driver.findElement(By.id("oldPass"));
    }

    // Поле "Новый пароль"
    public WebElement newPasswordField() {
        return _driver.findElement(By.id("newPass"));
    }

    // Поле "Подтвердите новый пароль"
    public WebElement confirmNewPasswordField() {
        return _driver.findElement(By.id("newPassConfirm"));
    }

    // Значение в поле "Старый пароль"
    public String oldPasswordValue() {
        _waiter.until(ExpectedConditions.elementToBeClickable(oldPasswordField()));
        return oldPasswordField().getText();
    }

    // Значение в поле "Новый пароль"
    public String newPasswordValue() {
        _waiter.until(ExpectedConditions.elementToBeClickable(newPasswordField()));
        return newPasswordField().getText();
    }

    // Значение в поле "Подтвердите новый пароль"
    public String confirmNewPasswordValue() {
        _waiter.until(ExpectedConditions.elementToBeClickable(confirmNewPasswordField()));
        return confirmNewPasswordField().getText();
    }

    // Кнопка "Изменить пароль"
    public WebElement changePasswordButton() {
        return _driver.findElement(By.xpath(".//*[@onclick=\"$.ils.changePass()\"]"));
    }

    // Кнопка "Закрыть"
    public WebElement closeButton() {
        return _driver.findElement(By.xpath(".//*[contains(@class, \"ui-button ui-corner-all ui-widget\")]"));
    }


    // --------------------------------------- UserProfileForm events ------------------------------------------

    // Ввод в поле "Имя пользователя"
    public void typeToAccountField(String account) {
        _waiter.until(ExpectedConditions.elementToBeClickable(accountNameField()));
        accountNameField().clear();
        accountNameField().sendKeys(account);
    }

    // Ввод в поле "Старый пароль"
    public void typeOldPassword(String oldPassword) {
        _waiter.until(ExpectedConditions.elementToBeClickable(oldPasswordField()));
        oldPasswordField().clear();
        oldPasswordField().sendKeys(oldPassword);
    }

    // Ввод в поле "Новый пароль"
    public void typeNewPassword(String newPassword) {
        _waiter.until(ExpectedConditions.elementToBeClickable(newPasswordField()));
        newPasswordField().clear();
        newPasswordField().sendKeys(newPassword);
    }

    // Ввод в поле "Подтверждение пароля"
    public void typeConfirmNewPassword(String confirmNewPassword) {
        _waiter.until(ExpectedConditions.elementToBeClickable(confirmNewPasswordField()));
        confirmNewPasswordField().clear();
        confirmNewPasswordField().sendKeys(confirmNewPassword);
    }

    // Нажатие кнопки "Закрыть"
    public void closeButtonClick() {
        _waiter.until(ExpectedConditions.elementToBeClickable(closeButton()));
        closeButton().click();
    }

    // Нажатие кнопки "Сохранить"
    public void saveButtonClick() {
        _waiter.until(ExpectedConditions.elementToBeClickable(saveButton()));
        saveButton().click();
    }

    // Нажатие кнопки "Изменить пароль"
    public void changePasswordButtonClick() {
        _waiter.until(ExpectedConditions.elementToBeClickable(changePasswordButton()));
        changePasswordButton().click();
    }




}
