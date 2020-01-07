/*
 * Copyright 2017-2020 the original author or authors.
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
 */

package org.springframework.data.gemfire.config.schema.definitions;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.IOException;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.query.Index;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import org.springframework.data.gemfire.IndexType;
import org.springframework.data.gemfire.config.admin.GemfireAdminOperations;
import org.springframework.data.gemfire.config.schema.SchemaObjectType;
import org.springframework.data.gemfire.test.support.IOUtils;

/**
 * Unit tests for {@link IndexDefinition}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.query.Index
 * @see org.springframework.data.gemfire.IndexType
 * @see org.springframework.data.gemfire.config.schema.definitions.IndexDefinition
 * @since 2.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class IndexDefinitionUnitTests {

	@Mock
	private Index mockIndex;

	@Mock
	private Region<?, ?> mockRegion;

	@Before
	public void setup() {
		when(this.mockIndex.getName()).thenReturn("MockIndex");
		when(this.mockRegion.getFullPath()).thenReturn(String.format("%sMockRegion", Region.SEPARATOR));
	}

	private Index mockIndex(String name, String expression, String fromClause, IndexType type) {

		Index mockIndex = mock(Index.class);

		when(mockIndex.getName()).thenReturn(name);
		when(mockIndex.getIndexedExpression()).thenReturn(expression);
		when(mockIndex.getFromClause()).thenReturn(fromClause);
		when(mockIndex.getType()).thenReturn(type.getGemfireIndexType());

		return mockIndex;
	}

	@Test
	public void fromIndexCreatesAnIndexDefinition() {

		Index mockIndex = mockIndex("TestIndex", "id", "/Customers", IndexType.PRIMARY_KEY);

		IndexDefinition indexDefinition = IndexDefinition.from(mockIndex);

		assertThat(indexDefinition).isNotNull();
		assertThat(indexDefinition.getExpression()).isEqualTo(mockIndex.getIndexedExpression());
		assertThat(indexDefinition.getFromClause()).isEqualTo(mockIndex.getFromClause());
		assertThat(indexDefinition.getIndex()).isSameAs(mockIndex);
		assertThat(indexDefinition.getIndexType()).isEqualTo(IndexType.valueOf(mockIndex.getType()));
		assertThat(indexDefinition.getType()).isEqualTo(SchemaObjectType.INDEX);
	}

	@Test(expected = IllegalArgumentException.class)
	public void fromNullIndexThrowsIllegalArgumentException() {

		try {
			IndexDefinition.from(null);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("Index is required");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	public void createCallsGemfireAdminOperationsCreateIndexWithThis() {

		GemfireAdminOperations mockAdminOperations = mock(GemfireAdminOperations.class);

		IndexDefinition indexDefinition = IndexDefinition.from(this.mockIndex);

		assertThat(indexDefinition).isNotNull();
		assertThat(indexDefinition.getIndex()).isSameAs(this.mockIndex);

		indexDefinition.create(mockAdminOperations);

		verify(mockAdminOperations, times(1)).createIndex(eq(indexDefinition));
	}

	@Test
	@SuppressWarnings("deprecation")
	public void asDifferentIndexTypes() {

		when(this.mockIndex.getType()).thenReturn(IndexType.FUNCTIONAL.getGemfireIndexType());

		IndexDefinition indexDefinition = IndexDefinition.from(this.mockIndex);

		assertThat(indexDefinition).isNotNull();
		assertThat(indexDefinition.getIndex()).isSameAs(this.mockIndex);
		assertThat(indexDefinition.getIndexType()).isEqualTo(IndexType.FUNCTIONAL);
		assertThat(indexDefinition.as(IndexType.HASH)).isSameAs(indexDefinition);
		assertThat(indexDefinition.getIndexType()).isEqualTo(IndexType.HASH);
		assertThat(indexDefinition.as(org.apache.geode.cache.query.IndexType.PRIMARY_KEY)).isSameAs(indexDefinition);
		assertThat(indexDefinition.getIndexType()).isEqualTo(IndexType.PRIMARY_KEY);
	}

	@Test
	public void asNullIndexType() {

		when(this.mockIndex.getType()).thenReturn(IndexType.FUNCTIONAL.getGemfireIndexType());

		IndexDefinition indexDefinition = IndexDefinition.from(this.mockIndex);

		assertThat(indexDefinition).isNotNull();
		assertThat(indexDefinition.getIndex()).isSameAs(this.mockIndex);
		assertThat(indexDefinition.getIndexType()).isEqualTo(IndexType.FUNCTIONAL);
		assertThat(indexDefinition.as((IndexType) null)).isSameAs(indexDefinition);
		assertThat(indexDefinition.getIndexType()).isEqualTo(IndexType.FUNCTIONAL);
		assertThat(indexDefinition.as(IndexType.HASH)).isSameAs(indexDefinition);
		assertThat(indexDefinition.getIndexType()).isEqualTo(IndexType.HASH);
		assertThat(indexDefinition.as((IndexType) null)).isSameAs(indexDefinition);
		assertThat(indexDefinition.getIndexType()).isEqualTo(IndexType.FUNCTIONAL);
	}

	private void testHavingIllegalExpression(String expression) {

		try {
			IndexDefinition.from(this.mockIndex).having(expression);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("Expression is required");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	public void havingExpression() {

		when(this.mockIndex.getIndexedExpression()).thenReturn("id");

		IndexDefinition indexDefinition = IndexDefinition.from(this.mockIndex);

		assertThat(indexDefinition).isNotNull();
		assertThat(indexDefinition.getIndex()).isSameAs(this.mockIndex);
		assertThat(indexDefinition.getExpression()).isEqualTo("id");
		assertThat(indexDefinition.having("age")).isSameAs(indexDefinition);
		assertThat(indexDefinition.getExpression()).isEqualTo("age");
	}

	@Test(expected = IllegalArgumentException.class)
	public void havingEmptyExpression() {
		testHavingIllegalExpression("");
	}

	@Test(expected = IllegalArgumentException.class)
	public void havingNoExpression() {
		testHavingIllegalExpression("   ");
	}

	@Test(expected = IllegalArgumentException.class)
	public void havingNullExpression() {
		testHavingIllegalExpression(null);
	}

	private void testOnIllegalFromClause(String fromClause) {

		try {
			IndexDefinition.from(this.mockIndex).on(fromClause);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("From Clause is required");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	public void onFromClause() {

		when(this.mockIndex.getFromClause()).thenReturn("/People");

		IndexDefinition indexDefinition = IndexDefinition.from(this.mockIndex);

		assertThat(indexDefinition).isNotNull();
		assertThat(indexDefinition.getFromClause()).isEqualTo(this.mockIndex.getFromClause());
		assertThat(indexDefinition.on("/Customers")).isSameAs(indexDefinition);
		assertThat(indexDefinition.getFromClause()).isEqualTo("/Customers");
	}

	@Test(expected = IllegalArgumentException.class)
	public void onEmptyFromClause() {
		testOnIllegalFromClause("");
	}

	@Test(expected = IllegalArgumentException.class)
	public void onNoFromClause() {
		testOnIllegalFromClause("   ");
	}

	@Test(expected = IllegalArgumentException.class)
	public void onNullFromClause() {
		testOnIllegalFromClause(null);
	}

	@Test
	public void onRegion() {

		when(this.mockIndex.getFromClause()).thenReturn("/Mock");

		IndexDefinition indexDefinition = IndexDefinition.from(this.mockIndex);

		assertThat(indexDefinition).isNotNull();
		assertThat(indexDefinition.getFromClause()).isEqualTo(this.mockIndex.getFromClause());
		assertThat(indexDefinition.on(this.mockRegion)).isSameAs(indexDefinition);
		assertThat(indexDefinition.getFromClause()).isEqualTo(this.mockRegion.getFullPath());
	}

	@Test(expected = IllegalArgumentException.class)
	public void onNullRegion() {

		try {
			IndexDefinition.from(this.mockIndex).on((Region) null);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("Region is required");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	public void withName() {

		IndexDefinition indexDefinition = IndexDefinition.from(this.mockIndex);

		assertThat(indexDefinition).isNotNull();
		assertThat(indexDefinition.getName()).isEqualTo("MockIndex");
		assertThat(indexDefinition.with("TestIndex")).isSameAs(indexDefinition);
		assertThat(indexDefinition.getName()).isEqualTo("TestIndex");
		assertThat(indexDefinition.with("")).isSameAs(indexDefinition);
		assertThat(indexDefinition.getName()).isEqualTo("MockIndex");
		assertThat(indexDefinition.with("UniqueIndex")).isSameAs(indexDefinition);
		assertThat(indexDefinition.getName()).isEqualTo("UniqueIndex");
		assertThat(indexDefinition.with("  ")).isSameAs(indexDefinition);
		assertThat(indexDefinition.getName()).isEqualTo("MockIndex");
		assertThat(indexDefinition.with("NonUniqueIndex")).isSameAs(indexDefinition);
		assertThat(indexDefinition.getName()).isEqualTo("NonUniqueIndex");
		assertThat(indexDefinition.with(null)).isSameAs(indexDefinition);
		assertThat(indexDefinition.getName()).isEqualTo("MockIndex");
	}

	@Test
	public void serializeDeserializeIsSuccessful() throws ClassNotFoundException, IOException {

		when(this.mockIndex.getIndexedExpression()).thenReturn("age");
		when(this.mockIndex.getFromClause()).thenReturn("/Customers");
		when(this.mockIndex.getType()).thenReturn(IndexType.FUNCTIONAL.getGemfireIndexType());

		IndexDefinition indexDefinition = IndexDefinition.from(this.mockIndex);

		byte[] indexDefinitionBytes = IOUtils.serializeObject(indexDefinition);

		IndexDefinition deserializedIndexDefinition = IOUtils.deserializeObject(indexDefinitionBytes);

		assertThat(deserializedIndexDefinition).isNotNull();
		assertThat(deserializedIndexDefinition).isNotSameAs(indexDefinition);
		assertThat(deserializedIndexDefinition.getIndex()).isInstanceOf(Index.class);
		assertThat(deserializedIndexDefinition.getIndex()).isNotSameAs(this.mockIndex);
		assertThat(deserializedIndexDefinition.getName()).isEqualTo("MockIndex");
		assertThat(deserializedIndexDefinition.getExpression()).isEqualTo("age");
		assertThat(deserializedIndexDefinition.getFromClause()).isEqualTo("/Customers");
		assertThat(deserializedIndexDefinition.getIndexType()).isEqualTo(IndexType.FUNCTIONAL);
	}
}
