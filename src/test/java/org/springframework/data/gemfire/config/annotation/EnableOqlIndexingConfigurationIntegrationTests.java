/*
 * Copyright 2017-2018 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.config.annotation;

import static org.assertj.core.api.Assertions.assertThat;

import javax.annotation.Resource;

import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.client.ClientRegionShortcut;
import org.apache.geode.cache.query.Index;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.FilterType;
import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.data.gemfire.IndexType;
import org.springframework.data.gemfire.mapping.annotation.Indexed;
import org.springframework.data.gemfire.test.model.Book;
import org.springframework.data.gemfire.test.model.Person;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Integration tests for {@link EnableIndexing} and the {@link Indexed} annotation.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.query.Index
 * @see org.springframework.data.gemfire.config.annotation.EnableIndexing
 * @see org.springframework.data.gemfire.mapping.annotation.Indexed
 * @see org.springframework.data.gemfire.test.model.Person
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringRunner
 * @since 2.0.3
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class EnableOqlIndexingConfigurationIntegrationTests {

	@Resource(name = "People")
	private Region<Long, Person> people;

	@Resource(name = "PeopleIdKeyIdx")
	private Index personIdKeyIndex;

	@Resource(name = "PeopleLastNameHashIdx")
	private Index personLastNameHashIndex;

	@Test
	public void peopleRegionIsSetupCorrectly() {

		assertThat(this.people).isNotNull();
		assertThat(this.people.getName()).isEqualTo("People");
		assertThat(this.people.getFullPath()).isEqualTo(GemfireUtils.toRegionPath("People"));
		assertThat(this.people.getAttributes()).isNotNull();
		assertThat(this.people.getAttributes().getDataPolicy()).isEqualTo(DataPolicy.NORMAL);
	}

	private void assertIndex(Index index, String name, String expression, String from, IndexType indexType) {

		assertThat(index).isNotNull();
		assertThat(index.getName()).isEqualTo(name);
		assertThat(index.getIndexedExpression()).isEqualTo(expression);
		assertThat(index.getFromClause()).isEqualTo(from);
		assertThat(index.getType()).isEqualTo(indexType.getGemfireIndexType());
	}

	/**
	 * @see <a href="https://jira.spring.io/browse/DATAGEODE-68">From clause Region path error occurs when creating Indexes from application domain object fields annotated with @Indexed or @Id</a>
	 */
	@Test
	public void idKeyIndexAndLastNameHashIndexAreSetupCorrectly() {

		assertIndex(this.personIdKeyIndex, "PeopleIdKeyIdx", "id", "/People", IndexType.KEY);
		assertIndex(this.personLastNameHashIndex, "PeopleLastNameHashIdx",
			"lastName", "/People", IndexType.HASH);
	}

	@ClientCacheApplication(logLevel = "none")
	@EnableEntityDefinedRegions(
		basePackageClasses = Person.class,
		clientRegionShortcut = ClientRegionShortcut.LOCAL,
		excludeFilters = @ComponentScan.Filter(type = FilterType.ASSIGNABLE_TYPE, classes = Book.class)
	)
	@EnableIndexing
	static class TestConfiguration { }

}
