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
        return oldPasswordField().getText();
    }

    // �������� � ���� "����� ������"
    public String newPasswordValue() {
        return newPasswordField().getText();
    }

    // �������� � ���� "����������� ����� ������"
    public String confirmNewPasswordValue() {
        return confirmNewPasswordField().getText();
    }

    // ������ "�������� ������"
    public WebElement changePasswordButton() {
        return _driver.findElement(By.xpath(".//*[@onclick=\"$.ils.changePass()\"]"));
    }

    // ������ "�������"
    public WebElement closeButton() {
        return _driver.findElement(By.xpath(".//*[@class=\"ui-button ui-corner-all ui-widget\"]"));
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
        oldPasswordField().clear();
        oldPasswordField().sendKeys(oldPassword);
    }

    // ���� � ���� "����� ������"
    public void typeNewPassword(String newPassword) {
        newPasswordField().clear();
        newPasswordField().sendKeys(newPassword);
    }

    // ���� � ���� "������������� ������"
    public void typeConfirmNewPassword(String confirmNewPassword) {
        confirmNewPasswordField().clear();
        confirmNewPasswordField().sendKeys(confirmNewPassword);
    }

    // ������� ������ "�������"
    public void closeButtonClick() {
        closeButton().click();
    }

    // ������� ������ "���������"
    public void saveButtonClick() {
        saveButton().click();
    }

    // ������� ������ "�������� ������"
    public void changePasswordButtonClick() {
        changePasswordButton().click();
    }




}
