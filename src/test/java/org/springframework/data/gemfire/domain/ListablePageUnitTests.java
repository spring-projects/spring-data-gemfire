/*
 * Copyright 2016-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.springframework.data.gemfire.domain;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.data.gemfire.domain.ListablePage.newListablePage;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.junit.Test;

import org.springframework.data.domain.Page;

/**
 * Unit tests for {@link ListablePage}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.domain.ListablePage
 * @since 1.0.0
 */
public class ListablePageUnitTests {

	@Test
	public void newListablePageWithArray() {
		ListablePage<String> page = newListablePage("one", "two", "three");

		assertThat(page).isNotNull();
		assertThat(page).contains("one", "two", "three");
	}

	@Test
	public void newListablePageWithList() {
		ListablePage<Integer> page = newListablePage(Arrays.asList(1, 2, 3));

		assertThat(page).isNotNull();
		assertThat(page).contains(1, 2, 3);
	}

	@Test
	public void newListablePageWithNull() {
		ListablePage<Object> page = newListablePage((List<Object>) null);

		assertThat(page).isNotNull();
		assertThat(page).isEmpty();
	}

	@Test
	public void listablePageWithContentHasCorrectState() {
		List<Object> content = Arrays.asList(1, 2, 3);
		ListablePage<Object> page = newListablePage(content);

		assertThat(page).isNotNull();
		assertThat(page).isNotEmpty();
		assertThat(page).containsAll(content);
		assertThat(page.hasContent()).isTrue();
		assertThat(page.hasNext()).isFalse();
		assertThat(page.hasPrevious()).isFalse();
		assertThat(page.isFirst()).isTrue();
		assertThat(page.isLast()).isTrue();
		assertThat(page.getContent()).isEqualTo(content);
		assertThat(page.getNumber()).isEqualTo(1);
		assertThat(page.getNumberOfElements()).isEqualTo(content.size());
		assertThat(page.getSize()).isEqualTo(content.size());
		assertThat(page.getSort()).isNull();
		assertThat(page.getTotalElements()).isEqualTo(content.size());
		assertThat(page.getTotalPages()).isEqualTo(1);
	}

	@Test
	public void listablePageWithNoContentHasCorrectState() {
		ListablePage<Object> page = newListablePage();

		assertThat(page).isNotNull();
		assertThat(page).isEmpty();
		assertThat(page.hasContent()).isFalse();
		assertThat(page.hasNext()).isFalse();
		assertThat(page.hasPrevious()).isFalse();
		assertThat(page.isFirst()).isTrue();
		assertThat(page.isLast()).isTrue();
		assertThat(page.getContent()).isEqualTo(Collections.emptyList());
		assertThat(page.getNumber()).isEqualTo(1);
		assertThat(page.getNumberOfElements()).isEqualTo(0);
		assertThat(page.getSize()).isEqualTo(0);
		assertThat(page.getSort()).isNull();
		assertThat(page.getTotalElements()).isEqualTo(0);
		assertThat(page.getTotalPages()).isEqualTo(1);
	}

	@Test
	public void iterationIsCorrect() {
		ListablePage<Object> page = newListablePage(1, 2, 3);
		List<Object> elements = new ArrayList<>(page.getSize());

		for (Object element : page) {
			elements.add(element);
		}

		assertThat(elements).isEqualTo(page.getContent());
	}

	@Test
	public void mapWithConvertersIsCorrect() {
		ListablePage<Object> page = newListablePage("1", "2", "3");

		assertThat(page).isNotNull();
		assertThat(page).hasSize(3);

		Page<Integer> integersPage = page.map(value -> Integer.parseInt(String.valueOf(value)));

		assertThat(integersPage).isNotNull();
		assertThat(integersPage).contains(1, 2, 3);

		Page<Double> doublesPage = integersPage.map(Integer::doubleValue);

		assertThat(doublesPage).isNotNull();
		assertThat(doublesPage).contains(1.0d, 2.0d, 3.0d);
	}
}
