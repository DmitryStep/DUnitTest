package utils;

import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;
import java.util.List;

// Класс для работы с элементом интерфейса "Таблица"
public class TableManager {

    private static WebElement _table;

    public TableManager(WebElement Table) {
        _table = Table;
    }

    // Левый блок
    public WebElement LeftBlock() {
        return _table.findElement(By.className("TMBodyLeft"));
    }

    // Средний блок
    public WebElement MiddleBlock() {
        return _table.findElement(By.className("TMBodyMid"));
    }

    // Правый блок
    public WebElement RightBlock() {
        return _table.findElement(By.className("TMBodyRight"));
    }

    // Строки с данными
    public List<WebElement> getTableStrings(WebElement TableBlock) {
        return TableBlock.findElements(By.xpath("//table[@class=\"TMSection\"]/tbody/tr"));
    }

    // Количество строк с данными
    public int getTableStringsCount() {
        int tsCount;
        try {
            List<WebElement> tab = getTableStrings(LeftBlock());
            tsCount = tab.size() - 1;
        } catch (Exception e) {
            tsCount = 0;
        }
        return tsCount;
    }

}
