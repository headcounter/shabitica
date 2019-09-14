def test_main_page(selenium, shabitica):
    selenium.get(shabitica.main_url)
    selenium.implicitly_wait(20)
    selenium.find_element_by_id('app')
    selenium.find_element_by_class_name('logo')
    heading = selenium.find_element_by_tag_name('h1')
    assert 'Motivate yourself to achieve your goals.' in heading.text
