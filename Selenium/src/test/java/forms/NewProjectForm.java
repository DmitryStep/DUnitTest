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

    // Текст заголовка формы
    public String newProjectFormHeaderLabel() {
        return _driver.findElement(By.id("ui-id-1")).getText();
    }

    // Поле ввода "Название"
    public WebElement projectNameField() {
        return _driver.findElement(By.name("name"));
    }

    // Текст в поле "Название"
    public String nameFieldValue() {
        return projectNameField().getAttribute("value").toString();
    }

    // Поле даты
    public WebElement dateField() {
        return _driver.findElement(By.name("date"));
    }

    // Значение в поле даты
    public String dateFieldValue() {
        return dateField().getAttribute("value").toString();
    }

    // Кнопка "Создать"
    public WebElement createButton() {
        return _driver.findElement(By.xpath("[@class=\"ui-dialog-buttonset\"]/button[1]"));
    }

    // Кнопка "Отмена"
    public WebElement cancelButton() {
        return _driver.findElement(By.xpath("[@class=\"ui-dialog-buttonset\"]/button[2]"));
    }


    // --------------------------------------- NewProjectForm events ----------------------------------------

    // Ввод текста в поле "Название"
    public void typeToProjectNameField(String text) {
        projectNameField().clear();
        projectNameField().sendKeys(text);
    }

    // Ввод даты
    public void typeToDateField(String date) {
        dateField().clear();
        dateField().sendKeys(date);
    }

    // Нажать на кнопку "Отмена"

}
