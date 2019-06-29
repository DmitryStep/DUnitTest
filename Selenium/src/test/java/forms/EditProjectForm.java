package forms;

import baseclasses.BasePage;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;

public class EditProjectForm extends BasePage {

    public EditProjectForm(WebDriver driver) {
        super(driver);
    }

    // ����� ��������� �����
    public String editProjectFormHeaderLabel() {
        return _driver.findElement(By.id("ui-id-2")).getText();
    }

    // ���� ID
    public String idField() {
        return _driver.findElement(By.className("id")).getText();
    }

    // ���� "��������"

    // ����� � ���� "��������"

    // ���� "������� ������"

    // ��������� ����� "������� ������"

    // ���� "������"

    // ����� � ���� "������"

    // �������� "������"

    // �������� "������"

    // ������ "���������"

    // ������ "������"

}
