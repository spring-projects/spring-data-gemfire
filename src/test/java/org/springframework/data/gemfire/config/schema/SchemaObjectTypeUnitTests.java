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

package org.springframework.data.gemfire.config.schema;

import static java.util.Arrays.stream;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.springframework.data.gemfire.util.CollectionUtils.asSet;

import java.util.Set;
import java.util.stream.Collectors;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.DiskStore;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.asyncqueue.AsyncEventQueue;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.client.Pool;
import org.apache.geode.cache.execute.Function;
import org.apache.geode.cache.lucene.LuceneIndex;
import org.apache.geode.cache.query.Index;
import org.apache.geode.cache.wan.GatewayReceiver;
import org.apache.geode.cache.wan.GatewaySender;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit tests for {@link SchemaObjectType}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.config.schema.SchemaObjectType
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class SchemaObjectTypeUnitTests {

	@Test
	public void objectTypesAreSetAndCorrect() {

		Set<Class<?>> expectedSchemaObjectTypes = asSet(AsyncEventQueue.class, Cache.class, ClientCache.class,
			DiskStore.class, Function.class, GatewayReceiver.class, GatewaySender.class, Index.class, LuceneIndex.class,
			Pool.class, Region.class, Void.class);

		Set<Class<?>> actualSchemaObjectTypes = stream(SchemaObjectType.values())
			.map(SchemaObjectType::getObjectType)
			.collect(Collectors.toSet());

		assertThat(actualSchemaObjectTypes).hasSameSizeAs(expectedSchemaObjectTypes);
		assertThat(actualSchemaObjectTypes).containsAll(expectedSchemaObjectTypes);
	}

	@Test
	public void fromClass() {
		stream(SchemaObjectType.values()).forEach(it ->
			assertThat(SchemaObjectType.from(it.getObjectType())).isSameAs(it));
	}

	@Test
	public void fromNullIsUnknown() {
		assertThat(SchemaObjectType.from(null)).isSameAs(SchemaObjectType.UNKNOWN);
		assertThat(SchemaObjectType.from((Object) null)).isSameAs(SchemaObjectType.UNKNOWN);
	}

	@Test
	public void fromObject() {
		stream(SchemaObjectType.values()).filter(it -> !SchemaObjectType.UNKNOWN.equals(it)).forEach(it ->
			assertThat(SchemaObjectType.from(mock(it.getObjectType()))).isSameAs(it));
	}

	@Test
	public void fromUntypedObjectIsUnknown() {
		assertThat(SchemaObjectType.from(new Object())).isSameAs(SchemaObjectType.UNKNOWN);
	}
}
