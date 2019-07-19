package pageobjects.forms;

import baseclasses.BasePage;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.WebDriverWait;

public class MessageForm extends BasePage {


    public MessageForm(WebDriver driver, WebDriverWait waiter) {
        super(driver, waiter);
    }

    // Сообщение
    public WebElement MessageDlg() {
        return _driver.findElement(By.id("dlg"));
    }

    // Текст сообщения
    public String getMessage() {
        _waiter.until(ExpectedConditions.elementToBeClickable(MessageDlg()));
        return MessageDlg().getText();
    }

}
