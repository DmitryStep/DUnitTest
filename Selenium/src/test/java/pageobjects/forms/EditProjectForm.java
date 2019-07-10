package pageobjects.forms;

import baseclasses.BasePage;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

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
