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
import static org.mockito.Mockito.*;

import java.lang.reflect.Method;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.runners.MockitoJUnitRunner;
import org.springframework.data.gemfire.mapping.GemfireMappingContext;
import org.springframework.data.gemfire.repository.Query;
import org.springframework.data.gemfire.repository.sample.Person;
import org.springframework.data.repository.core.RepositoryMetadata;

/**
 * 
 * @author Oliver Gierke
 */
@RunWith(MockitoJUnitRunner.class)
public class GemfireQueryMethodUnitTests {

	@Mock
	RepositoryMetadata metadata;

	@Test
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public void detectsAnnotatedQueryCorrectly() throws Exception {

		GemfireMappingContext context = new GemfireMappingContext();
		when(metadata.getDomainType()).thenReturn((Class) Person.class);
		when(metadata.getReturnedDomainClass(Mockito.any(Method.class))).thenReturn((Class) Person.class);

		GemfireQueryMethod method = new GemfireQueryMethod(Sample.class.getMethod("annotated"), metadata, context);
		assertThat(method.hasAnnotatedQuery(), is(true));
		assertThat(method.getAnnotatedQuery(), is("foo"));

		method = new GemfireQueryMethod(Sample.class.getMethod("annotatedButEmpty"), metadata, context);
		assertThat(method.hasAnnotatedQuery(), is(false));
		assertThat(method.getAnnotatedQuery(), is(nullValue()));

		method = new GemfireQueryMethod(Sample.class.getMethod("notAnnotated"), metadata, context);
		assertThat(method.hasAnnotatedQuery(), is(false));
		assertThat(method.getAnnotatedQuery(), is(nullValue()));
	}

	interface Sample {

		@Query("foo")
		void annotated();

		@Query("")
		void annotatedButEmpty();

		void notAnnotated();
	}
}
