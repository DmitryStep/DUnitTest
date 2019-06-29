package forms;

import baseclasses.BasePage;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;

public class NewProjectForm extends BasePage {


    public NewProjectForm(WebDriver driver) {
        super(driver);
    }

    // ------------------------------------------- NewProjectForm WebElements -------------------------------------

    // ����� ��������� �����
    public String newProjectFormHeaderLabel() {
        return _driver.findElement(By.id("ui-id-1")).getText();
    }

    // ���� ����� "��������"
    public WebElement projectNameField() {
        return _driver.findElement(By.name("name"));
    }

    // ����� � ���� "��������"
    public String nameFieldValue() {
        return projectNameField().getAttribute("value").toString();
    }

    // ���� ����
    public WebElement dateField() {
        return _driver.findElement(By.name("date"));
    }

    // �������� � ���� ����
    public String dateFieldValue() {
        return dateField().getAttribute("value").toString();
    }

    // ������ "�������"
    public WebElement createButton() {
        return _driver.findElement(By.xpath("[@class=\"ui-dialog-buttonset\"]/button[1]"));
    }

    // ������ "������"
    public WebElement cancelButton() {
        return _driver.findElement(By.xpath("[@class=\"ui-dialog-buttonset\"]/button[2]"));
    }


    // --------------------------------------- NewProjectForm events ----------------------------------------

    // ���� ������ � ���� "��������"
    public void typeToProjectNameField(String text) {
        projectNameField().clear();
        projectNameField().sendKeys(text);
    }

    // ���� ����
    public void typeToDateField(String date) {
        dateField().clear();
        dateField().sendKeys(date);
    }

    // ������ �� ������ "������"

}
