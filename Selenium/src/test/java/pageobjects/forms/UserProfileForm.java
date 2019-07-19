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

    // ������� �� ���������
    public String userProfileFormHeaderLabel() {
        return _driver.findElement(By.id("ui-id-3")).getText();
    }

    // ���� "��� ������������"
    public WebElement accountNameField() {
        return _driver.findElement(By.id("accName"));
    }

    // �������� ���� "��� ������������"
    public String userFieldValue() {
        return accountNameField().getAttribute("value").toString();
    }

    // ������ "���������"
    public WebElement saveButton() {
        return _driver.findElement(By.xpath(".//*[@onclick=\"$.ils.renameAccount()\"]"));
    }

    // ���� "������ ������"
    public WebElement oldPasswordField() {
        return _driver.findElement(By.id("oldPass"));
    }

    // ���� "����� ������"
    public WebElement newPasswordField() {
        return _driver.findElement(By.id("newPass"));
    }

    // ���� "����������� ����� ������"
    public WebElement confirmNewPasswordField() {
        return _driver.findElement(By.id("newPassConfirm"));
    }

    // �������� � ���� "������ ������"
    public String oldPasswordValue() {
        _waiter.until(ExpectedConditions.elementToBeClickable(oldPasswordField()));
        return oldPasswordField().getText();
    }

    // �������� � ���� "����� ������"
    public String newPasswordValue() {
        _waiter.until(ExpectedConditions.elementToBeClickable(newPasswordField()));
        return newPasswordField().getText();
    }

    // �������� � ���� "����������� ����� ������"
    public String confirmNewPasswordValue() {
        _waiter.until(ExpectedConditions.elementToBeClickable(confirmNewPasswordField()));
        return confirmNewPasswordField().getText();
    }

    // ������ "�������� ������"
    public WebElement changePasswordButton() {
        return _driver.findElement(By.xpath(".//*[@onclick=\"$.ils.changePass()\"]"));
    }

    // ������ "�������"
    public WebElement closeButton() {
        return _driver.findElement(By.xpath(".//*[contains(@class, \"ui-button ui-corner-all ui-widget\")]"));
    }


    // --------------------------------------- UserProfileForm events ------------------------------------------

    // ���� � ���� "��� ������������"
    public void typeToAccountField(String account) {
        _waiter.until(ExpectedConditions.elementToBeClickable(accountNameField()));
        accountNameField().clear();
        accountNameField().sendKeys(account);
    }

    // ���� � ���� "������ ������"
    public void typeOldPassword(String oldPassword) {
        _waiter.until(ExpectedConditions.elementToBeClickable(oldPasswordField()));
        oldPasswordField().clear();
        oldPasswordField().sendKeys(oldPassword);
    }

    // ���� � ���� "����� ������"
    public void typeNewPassword(String newPassword) {
        _waiter.until(ExpectedConditions.elementToBeClickable(newPasswordField()));
        newPasswordField().clear();
        newPasswordField().sendKeys(newPassword);
    }

    // ���� � ���� "������������� ������"
    public void typeConfirmNewPassword(String confirmNewPassword) {
        _waiter.until(ExpectedConditions.elementToBeClickable(confirmNewPasswordField()));
        confirmNewPasswordField().clear();
        confirmNewPasswordField().sendKeys(confirmNewPassword);
    }

    // ������� ������ "�������"
    public void closeButtonClick() {
        _waiter.until(ExpectedConditions.elementToBeClickable(closeButton()));
        closeButton().click();
    }

    // ������� ������ "���������"
    public void saveButtonClick() {
        _waiter.until(ExpectedConditions.elementToBeClickable(saveButton()));
        saveButton().click();
    }

    // ������� ������ "�������� ������"
    public void changePasswordButtonClick() {
        _waiter.until(ExpectedConditions.elementToBeClickable(changePasswordButton()));
        changePasswordButton().click();
    }




}
