/*
 * Copyright 2012 the original author or authors.
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
package org.springframework.data.gemfire.repository.query;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import org.junit.Before;
import org.junit.Test;
import org.springframework.data.gemfire.mapping.GemfireMappingContext;
import org.springframework.data.gemfire.mapping.GemfirePersistentEntity;
import org.springframework.data.gemfire.repository.sample.Person;
import org.springframework.data.repository.query.parser.PartTree;

/**
 * Unit tests for {@link GemfireQueryCreator}.
 * 
 * @author Oliver Gierke
 */
public class GemfireQueryCreatorUnitTests {

	GemfirePersistentEntity<Person> entity;

	@Before
	@SuppressWarnings("unchecked")
	public void setUp() {

		GemfireMappingContext context = new GemfireMappingContext();
		entity = (GemfirePersistentEntity<Person>) context.getPersistentEntity(Person.class);
	}

	@Test
	public void createsQueryForSimplePropertyReferenceCorrectly() {

		PartTree partTree = new PartTree("findByFirstname", Person.class);
		GemfireQueryCreator creator = new GemfireQueryCreator(partTree, entity);

		QueryString query = creator.createQuery();
		assertThat(query.toString(), is("SELECT * FROM /simple x WHERE x.firstname = $1"));
	}
}
