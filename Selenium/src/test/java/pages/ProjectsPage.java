package pages;

import baseclasses.BasePage;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;

public class ProjectsPage extends BasePage {

    // ---------------------------------- Page Constructor ------------------------------------------------------

    public ProjectsPage(WebDriver driver){
        super(driver);
    }

    // ---------------------------------- ProjectsPage WebElements ---------------------------------------------

    // Кнопка "Создать новый проект"
    public WebElement newProjectButton(){
        return _driver.findElement(By.xpath(".//*[@id=\"ils-body\"]/div/div[1]/button"));
    }

    // Дата начала периода фильтра
    public WebElement beginDate() {
        return _driver.findElement(By.xpath(".//*[@id=\"project_date1\"]"));
    }

    // Дата окончания периода фильтра
    public WebElement endDate() {
        return _driver.findElement(By.xpath(".//*[@id=\"project_date2\"]"));
    }

    // Кнопка "Показать фильтр
    public WebElement showFilterResults() {
        return _driver.findElement(By.xpath(".//*[@id=\"ils-body\"]/div/div[2]/div[2]/button"));
    }

    // Таблица проектов
    public WebElement projectsTable() {
        return  _driver.findElement(By.xpath(".//*[@id=\"ils-body\"]/div/table"));
    }


}
