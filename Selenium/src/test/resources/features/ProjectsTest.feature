#language: ru

Функция: Управление проектами

  @debug
  Сценарий: Открытие страницы проектов
    Дано  Пользователь авторизован в системе
    Если  Нажать ILS Plan
    Тогда Количество проектов в таблице = 2