package utils;

import org.openqa.selenium.WebElement;

// ����� ��� ������ � ��������� ���������� "�������"
public class TableManager {

    private static WebElement _table;

    public TableManager(WebElement Table) {
        _table = Table;
    }

}
