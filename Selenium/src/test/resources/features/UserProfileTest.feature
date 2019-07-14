#language: ru

Функция: Профиль текущего пользователя

  @userprofile
  Структура сценария: Смена пароля - негативный кейс: старый пароль <Старый пароль>, новый пароль <Новый пароль>, подтверждение пароля <Подтверждение пароля>. ОР: сообщение <Текст ошибки>

    Дано  Адрес = http://test.intelogis.ru
       И  Пользователь авторизован с логином 1@test.ils и паролем 123
    Если  Нажать меню пользователя
       И  Выбрать пункт меню Редактировать профиль
       И  Ввести старый пароль <Старый пароль>
       И  Ввести новый пароль <Новый пароль>
       И  Ввести подтверждение пароля <Подтверждение пароля>
       И  Нажать Изменить пароль
    Тогда Ошибка <Текст ошибки>
    Примеры:
      | Старый пароль | Новый пароль | Подтверждение пароля | Текст ошибки                      |
      | 1234          |              | 1                    | Необходимо заполнить все поля     |
      | asdf          | dsfa         | dsfa                 | Указан не верный пароль           |
      | 1111          |              |                      | Необходимо заполнить все поля     |
      | aaaa          | aaaa         | aaaa                 | Указан не верный пароль           |
      |               |              |                      | Необходимо заполнить все поля     |
      |               | 1234         | 4321                 | Необходимо заполнить все поля     |
      | 4321          | 1234         |                      | Необходимо заполнить все поля     |
      | 4321          | 1234         | 4333                 | Подтверждение пароля не совпадает |
      | 4321          |              |                      | Необходимо заполнить все поля     |

  @userprofile
  Сценарий: Смена аккаунта - негативный кейс: смена на пустое значение. ОР: сообщение Не указано название аккаунта
      Дано  Пользователь авторизован в системе
      Если  Нажать меню пользователя
         И  Выбрать пункт меню Редактировать профиль
         И  Очистить аккаунт
      Тогда Ошибка Не указано название аккаунта

  @userprofile
  Структура сценария: Смена аккаунта и пароля на <Новый аккаунт> : <Новый пароль>. ОР: после смены пользователь авторизуется с новыми данными

      Дано  Адрес = http://test.intelogis.ru
         И  Пользователь авторизован с логином 1@test.ils и паролем <Старый пароль>
      Если  Нажать меню пользователя
         И  Выбрать пункт меню Редактировать профиль
         И  Изменить аккаунт на <Новый аккаунт>
         И  Изменить пароль <Старый пароль> на <Новый пароль>
         И  Закрыть форму профиля пользователя
         И  Обновить страницу
         И  Нажать меню пользователя
         И  Выбрать пункт меню Выход
         И  Авторизоваться с логином 1@test.ils и паролем <Новый пароль>
      Тогда Текущая URL = http://test.intelogis.ru/
          И Аккаунт пользователя = <Новый аккаунт>
          И Заголовок страницы = Система мониторинга и планирования логистики
          И Активный язык Русский
    Примеры:
      | Старый пароль | Новый пароль | Новый аккаунт |
      | 123           | 1234         | Test          |
      | 1234          | 123          | 1@test.ils    |
