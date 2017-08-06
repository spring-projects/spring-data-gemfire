/*
 * Copyright 2017 the original author or authors.
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

package org.springframework.data.gemfire.config.schema.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.data.gemfire.util.CollectionUtils.asSet;

import java.util.Collections;

import org.apache.geode.cache.GemFireCache;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.config.schema.SchemaObjectCollector;
import org.springframework.data.gemfire.config.schema.SchemaObjectType;

import lombok.NonNull;
import lombok.RequiredArgsConstructor;

/**
 * Unit tests for {@link ComposableSchemaObjectCollector}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.config.schema.support.ComposableSchemaObjectCollector
 * @since 2.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class ComposableSchemaObjectCollectorUnitTests {

	@Mock
	private ApplicationContext mockApplicationContext;

	@Mock
	private GemFireCache mockCache;

	@Mock
	private SchemaObjectCollector<Object> mockSchemaObjectCollectorOne;

	@Mock
	private SchemaObjectCollector<Object> mockSchemaObjectCollectorTwo;

	private <T> Iterable<T> emptyIterable() {
		return Collections::emptyIterator;
	}

	@Test
	public void composeArrayWithNoElements() {
		assertThat(ComposableSchemaObjectCollector.compose()).isNull();
	}

	@Test
	public void composeArrayWithOneElement() {
		assertThat(ComposableSchemaObjectCollector.compose(this.mockSchemaObjectCollectorOne))
			.isSameAs(this.mockSchemaObjectCollectorOne);
	}

	@Test
	public void composeArrayWithTwoElements() {

		SchemaObjectCollector<?> composedSchemaObjectCollector = ComposableSchemaObjectCollector.compose(
			this.mockSchemaObjectCollectorOne, this.mockSchemaObjectCollectorTwo);

		assertThat(composedSchemaObjectCollector).isInstanceOf(ComposableSchemaObjectCollector.class);
		assertThat((ComposableSchemaObjectCollector) composedSchemaObjectCollector).hasSize(2);
		assertThat((ComposableSchemaObjectCollector) composedSchemaObjectCollector)
			.containsAll(asSet(this.mockSchemaObjectCollectorOne, this.mockSchemaObjectCollectorTwo));

	}

	@Test
	public void composeIterableWithNoElements() {
		assertThat(ComposableSchemaObjectCollector.compose(emptyIterable())).isNull();
	}

	@Test
	public void composableIterableWithOneElement() {
		assertThat(ComposableSchemaObjectCollector.compose(Collections.singleton(this.mockSchemaObjectCollectorTwo)))
			.isSameAs(this.mockSchemaObjectCollectorTwo);
	}

	@Test
	public void composeIterableWithTwoElements() {

		SchemaObjectCollector<?> composedSchemaObjectCollector = ComposableSchemaObjectCollector.compose(
			asSet(this.mockSchemaObjectCollectorOne, this.mockSchemaObjectCollectorTwo));

		assertThat(composedSchemaObjectCollector).isInstanceOf(ComposableSchemaObjectCollector.class);
		assertThat((ComposableSchemaObjectCollector) composedSchemaObjectCollector)
			.containsAll(asSet(this.mockSchemaObjectCollectorOne, this.mockSchemaObjectCollectorTwo));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void collectFromApplicationContext() {

		SchemaObject region = SchemaObject.of(SchemaObjectType.REGION);
		SchemaObject index = SchemaObject.of(SchemaObjectType.INDEX);
		SchemaObject diskStore = SchemaObject.of(SchemaObjectType.DISK_STORE);

		when(this.mockSchemaObjectCollectorOne.collectFrom(any(ApplicationContext.class)))
			.thenReturn(asSet(region, index));

		when(this.mockSchemaObjectCollectorTwo.collectFrom(any(ApplicationContext.class)))
			.thenReturn(asSet(diskStore));

		SchemaObjectCollector composedSchemaObjectCollector = ComposableSchemaObjectCollector.compose(
			asSet(this.mockSchemaObjectCollectorOne, this.mockSchemaObjectCollectorTwo));

		assertThat(composedSchemaObjectCollector).isNotNull();

		Iterable<Object> schemaObjects = composedSchemaObjectCollector.collectFrom(this.mockApplicationContext);

		assertThat(schemaObjects).isNotNull();
		assertThat(schemaObjects).hasSize(3);
		assertThat(schemaObjects).contains(region, index, diskStore);

		verify(this.mockSchemaObjectCollectorOne, times(1))
			.collectFrom(eq(this.mockApplicationContext));

		verify(this.mockSchemaObjectCollectorTwo, times(1))
			.collectFrom(eq(this.mockApplicationContext));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void collectFromGemFireCache() {

		SchemaObject asyncEventQueue = SchemaObject.of(SchemaObjectType.ASYNC_EVENT_QUEUE);
		SchemaObject gatewayReceiver = SchemaObject.of(SchemaObjectType.GATEWAY_RECEIVER);
		SchemaObject gatewaySender = SchemaObject.of(SchemaObjectType.GATEWAY_SENDER);

		when(this.mockSchemaObjectCollectorOne.collectFrom(any(GemFireCache.class)))
			.thenReturn(asSet(gatewayReceiver, gatewaySender));

		when(this.mockSchemaObjectCollectorTwo.collectFrom(any(GemFireCache.class)))
			.thenReturn(asSet(asyncEventQueue));

		SchemaObjectCollector composedSchemaObjectCollector = ComposableSchemaObjectCollector.compose(
			asSet(this.mockSchemaObjectCollectorOne, this.mockSchemaObjectCollectorTwo));

		assertThat(composedSchemaObjectCollector).isNotNull();

		Iterable<Object> schemaObjects = composedSchemaObjectCollector.collectFrom(this.mockCache);

		assertThat(schemaObjects).isNotNull();
		assertThat(schemaObjects).hasSize(3);
		assertThat(schemaObjects).contains(gatewayReceiver, gatewaySender, asyncEventQueue);

		verify(this.mockSchemaObjectCollectorOne, times(1)).collectFrom(eq(this.mockCache));

		verify(this.mockSchemaObjectCollectorTwo, times(1)).collectFrom(eq(this.mockCache));
	}

	@RequiredArgsConstructor(staticName = "of")
	static class SchemaObject {
		@NonNull SchemaObjectType type;
	}
}
