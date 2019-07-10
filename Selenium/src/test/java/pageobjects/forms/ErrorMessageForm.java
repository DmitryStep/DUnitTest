package pageobjects.forms;

import baseclasses.BasePage;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

public class ErrorMessageForm extends BasePage {


    public ErrorMessageForm(WebDriver driver) {
        super(driver);
    }

    // ����� ��������� �� ������
    public String getErrorMessage() {
        return _driver.findElement(By.id("dlg")).getText();
    }

}
