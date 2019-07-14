package pageobjects.forms;

import baseclasses.BasePage;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.support.ui.WebDriverWait;

public class MessageForm extends BasePage {


    public MessageForm(WebDriver driver, WebDriverWait waiter) {
        super(driver, waiter);
    }

    // Текст сообщения об ошибке
    public String getMessage() {
        return _driver.findElement(By.id("dlg")).getText();
    }

}
