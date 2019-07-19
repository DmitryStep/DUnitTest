package utils;

import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;
import java.util.List;

// ����� ��� ������ � ��������� ���������� "�������"
public class TableManager {

    private static WebElement _table;

    public TableManager(WebElement Table) {
        _table = Table;
    }

    // ����� ����
    public WebElement LeftBlock() {
        return _table.findElement(By.className("TMBodyLeft"));
    }

    // ������� ����
    public WebElement MiddleBlock() {
        return _table.findElement(By.className("TMBodyMid"));
    }

    // ������ ����
    public WebElement RightBlock() {
        return _table.findElement(By.className("TMBodyRight"));
    }

    // ������ � �������
    public List<WebElement> getTableStrings(WebElement TableBlock) {
        return TableBlock.findElements(By.xpath("//table[@class=\"TMSection\"]/tbody/tr"));
    }

    // ���������� ����� � �������
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
