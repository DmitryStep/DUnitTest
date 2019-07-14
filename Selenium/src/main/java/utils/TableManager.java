package utils;

import org.openqa.selenium.WebElement;

// Класс для работы с элементом интерфейса "Таблица"
public class TableManager {

    private static WebElement _table;

    public TableManager(WebElement Table) {
        _table = Table;
    }

}
