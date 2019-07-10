package pageobjects.forms;

import baseclasses.BasePage;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

public class EditProjectForm extends BasePage {

    public EditProjectForm(WebDriver driver) {
        super(driver);
    }

    // Текст заголовка формы
    public String editProjectFormHeaderLabel() {
        return _driver.findElement(By.id("ui-id-2")).getText();
    }

    // Поле ID
    public String idField() {
        return _driver.findElement(By.className("id")).getText();
    }

    // Поле "Название"

    // Текст в поле "Название"

    // Флаг "Главный проект"

    // Состояние флага "Главный проект"

    // Поле "Начало"

    // Текст в поле "Начало"

    // Значение "Создан"

    // Значение "Изменён"

    // Кнопка "Сохранить"

    // Кнопка "Отмена"

}
