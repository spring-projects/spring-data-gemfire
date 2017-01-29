/*
 * Copyright 2012-2018 the original author or authors.
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

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.when;

import java.lang.reflect.Method;

import org.hamcrest.Matchers;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.gemfire.mapping.GemfireMappingContext;
import org.springframework.data.gemfire.repository.Query;
import org.springframework.data.gemfire.repository.query.annotation.Hint;
import org.springframework.data.gemfire.repository.query.annotation.Import;
import org.springframework.data.gemfire.repository.query.annotation.Limit;
import org.springframework.data.gemfire.repository.query.annotation.Trace;
import org.springframework.data.gemfire.repository.sample.Person;
import org.springframework.data.projection.ProjectionFactory;
import org.springframework.data.projection.SpelAwareProxyProjectionFactory;
import org.springframework.data.repository.core.RepositoryMetadata;
import org.springframework.util.ObjectUtils;

/**
 * Unit tests for {@link GemfireQueryMethod}.
 *
 * @author Oliver Gierke
 * @author John Blum
 */
@RunWith(MockitoJUnitRunner.class)
public class GemfireQueryMethodUnitTests {

	@Rule
	public ExpectedException expectedException = ExpectedException.none();

	private GemfireMappingContext context = new GemfireMappingContext();

	private ProjectionFactory factory = new SpelAwareProxyProjectionFactory();

	@Mock
	private RepositoryMetadata metadata;

	protected void assertQueryHints(GemfireQueryMethod queryMethod, String... expectedHints) {
		assertThat(queryMethod, is(not(nullValue())));
		assertThat(queryMethod.hasHint(), is(!ObjectUtils.isEmpty(expectedHints)));

		String[] actualHints = queryMethod.getHints();

		assertThat(actualHints, is(not(nullValue())));
		assertThat(actualHints.length, is(equalTo(expectedHints.length)));

		for (int index = 0; index < expectedHints.length; index++) {
			assertThat(actualHints[index], is(equalTo(expectedHints[index])));
		}
	}

	protected void assertNoQueryHints(GemfireQueryMethod queryMethod) {
		assertQueryHints(queryMethod);
	}

	protected void assertImportStatement(GemfireQueryMethod queryMethod, String expectedImport) {
		assertThat(queryMethod, is(not(nullValue())));
		assertThat(queryMethod.hasImport(), is(expectedImport != null));

		if (expectedImport != null) {
			assertThat(queryMethod.getImport(), is(equalTo(expectedImport)));
		}
		else {
			assertThat(queryMethod.getImport(), is(nullValue()));
		}
	}

	protected void assertNoImportStatement(GemfireQueryMethod queryMethod) {
		assertImportStatement(queryMethod, null);
	}

	protected void assertLimitedQuery(GemfireQueryMethod queryMethod, Integer expectedLimit) {
		assertThat(queryMethod, is(not(nullValue())));
		assertThat(queryMethod.hasLimit(), is(expectedLimit != null));

		if (expectedLimit != null) {
			assertThat(queryMethod.getLimit(), is(equalTo(expectedLimit)));
		}
		else {
			assertThat(queryMethod.getLimit(), is(equalTo(Integer.MAX_VALUE)));
		}
	}

	protected void assertUnlimitedQuery(GemfireQueryMethod queryMethod) {
		assertLimitedQuery(queryMethod, null);
	}

	@Before
	@SuppressWarnings("unchecked")
	public void setup() {
		when(metadata.getDomainType()).thenReturn((Class) Person.class);
		when(metadata.getReturnedDomainClass(Mockito.any(Method.class))).thenReturn((Class) Person.class);
	}

	@Test
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public void detectsAnnotatedQueryCorrectly() throws Exception {
		GemfireQueryMethod method = new GemfireQueryMethod(Sample.class.getMethod("annotated"), metadata, factory, context);

		assertThat(method.hasAnnotatedQuery(), is(true));
		assertThat(method.getAnnotatedQuery(), is("foo"));

		method = new GemfireQueryMethod(Sample.class.getMethod("annotatedButEmpty"), metadata, factory, context);

		assertThat(method.hasAnnotatedQuery(), is(false));
		assertThat(method.getAnnotatedQuery(), is(nullValue()));

		method = new GemfireQueryMethod(Sample.class.getMethod("notAnnotated"), metadata, factory, context);

		assertThat(method.hasAnnotatedQuery(), is(false));
		assertThat(method.getAnnotatedQuery(), is(nullValue()));
	}

	/**
	 * @link http://jira.spring.io/browse/SGF-112
	 */
	@Test
	public void rejectsQueryMethodWithPageableParameter() throws Exception {
		expectedException.expect(IllegalStateException.class);
		expectedException.expectCause(is(nullValue(Throwable.class)));
		expectedException.expectMessage(Matchers.startsWith("Pagination is not supported by GemFire Repositories; Offending method: someMethod"));

		new GemfireQueryMethod(Invalid.class.getMethod("someMethod", Pageable.class), metadata, factory, context);
	}

	@Test
	public void detectsQueryHintsCorrectly() throws Exception {
		assertThat(new GemfireQueryMethod(AnnotatedQueryMethods.class.getMethod("queryWithHint"),
			metadata, factory, context).hasHint(), is(true));
		assertThat(new GemfireQueryMethod(AnnotatedQueryMethods.class.getMethod("queryWithImport"),
			metadata, factory, context).hasHint(), is(false));
		assertThat(new GemfireQueryMethod(AnnotatedQueryMethods.class.getMethod("limitedQuery"),
			metadata, factory, context).hasHint(), is(true));
		assertThat(new GemfireQueryMethod(AnnotatedQueryMethods.class.getMethod("unlimitedQuery"),
			metadata, factory, context).hasHint(), is(false));
	}

	@Test
	public void detectsQueryImportsCorrectly() throws Exception {
		assertThat(new GemfireQueryMethod(AnnotatedQueryMethods.class.getMethod("queryWithHint"),
			metadata, factory, context).hasImport(), is(false));
		assertThat(new GemfireQueryMethod(AnnotatedQueryMethods.class.getMethod("queryWithImport"),
			metadata, factory, context).hasImport(), is(true));
		assertThat(new GemfireQueryMethod(AnnotatedQueryMethods.class.getMethod("limitedQuery"),
			metadata, factory, context).hasImport(), is(true));
		assertThat(new GemfireQueryMethod(AnnotatedQueryMethods.class.getMethod("unlimitedQuery"),
			metadata, factory, context).hasImport(), is(false));
	}

	@Test
	public void detectsQueryLimitsCorrectly() throws Exception {
		assertThat(new GemfireQueryMethod(AnnotatedQueryMethods.class.getMethod("queryWithHint"),
			metadata, factory, context).hasLimit(), is(false));
		assertThat(new GemfireQueryMethod(AnnotatedQueryMethods.class.getMethod("queryWithImport"),
			metadata, factory, context).hasLimit(), is(false));
		assertThat(new GemfireQueryMethod(AnnotatedQueryMethods.class.getMethod("limitedQuery"),
			metadata, factory, context).hasLimit(), is(true));
		assertThat(new GemfireQueryMethod(AnnotatedQueryMethods.class.getMethod("unlimitedQuery"),
			metadata, factory, context).hasLimit(), is(false));
	}

	@Test
	public void detectsQueryTracingCorrectly() throws Exception {
		assertThat(new GemfireQueryMethod(AnnotatedQueryMethods.class.getMethod("queryWithHint"),
			metadata, factory, context).hasTrace(), is(true));
		assertThat(new GemfireQueryMethod(AnnotatedQueryMethods.class.getMethod("queryWithImport"),
			metadata, factory, context).hasTrace(), is(false));
		assertThat(new GemfireQueryMethod(AnnotatedQueryMethods.class.getMethod("limitedQuery"),
			metadata, factory, context).hasTrace(), is(false));
		assertThat(new GemfireQueryMethod(AnnotatedQueryMethods.class.getMethod("unlimitedQuery"),
			metadata, factory, context).hasTrace(), is(true));
	}

	@Test
	public void hintOnQueryWithHint() throws Exception {
		assertQueryHints(new GemfireQueryMethod(AnnotatedQueryMethods.class.getMethod("queryWithHint"),
			metadata, factory, context), "IdIdx", "LastNameIdx");

		assertQueryHints(new GemfireQueryMethod(AnnotatedQueryMethods.class.getMethod("limitedQuery"),
			metadata, factory, context), "BirthDateIdx");
	}

	@Test
	public void hintOnQueryWithNoHints() throws Exception {
		assertNoQueryHints(new GemfireQueryMethod(AnnotatedQueryMethods.class.getMethod("queryWithImport"),
			metadata, factory, context));
	}

	@Test
	public void importOnQueryWithImport() throws Exception {
		assertImportStatement(new GemfireQueryMethod(AnnotatedQueryMethods.class.getMethod("queryWithImport"),
			metadata, factory, context), "org.example.app.domain.ExampleType");

		assertImportStatement(new GemfireQueryMethod(AnnotatedQueryMethods.class.getMethod("limitedQuery"),
			metadata, factory, context), "org.example.app.domain.Person");
	}

	@Test
	public void importOnQueryWithNoImports() throws Exception {
		assertNoImportStatement(new GemfireQueryMethod(AnnotatedQueryMethods.class.getMethod("queryWithHint"),
			metadata, factory, context));
	}

	@Test
	public void limitOnQueryWithLimit() throws Exception {
		assertLimitedQuery(new GemfireQueryMethod(AnnotatedQueryMethods.class.getMethod("limitedQuery"),
			metadata, factory, context), 1024);
	}

	@Test
	public void limitOnQueryWithNoLimits() throws Exception {
		assertUnlimitedQuery(new GemfireQueryMethod(AnnotatedQueryMethods.class.getMethod("unlimitedQuery"),
			metadata, factory, context));
	}

	@SuppressWarnings("unused")
	interface Sample {

		@Query("foo")
		void annotated();

		@Query("")
		void annotatedButEmpty();

		void notAnnotated();

	}

	@SuppressWarnings("unused")
	interface Invalid {

		Page<?> someMethod(Pageable pageable);

	}

	@SuppressWarnings("unused")
	interface AnnotatedQueryMethods {

		@Trace
		@Hint({ "IdIdx", "LastNameIdx" })
		void queryWithHint();

		@Import("org.example.app.domain.ExampleType")
		void queryWithImport();

		@Hint("BirthDateIdx")
		@Import("org.example.app.domain.Person")
		@Limit(1024)
		void limitedQuery();

		@Trace
		void unlimitedQuery();

	}
}
