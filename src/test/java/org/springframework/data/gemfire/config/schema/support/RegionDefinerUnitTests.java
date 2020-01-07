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

package org.springframework.data.gemfire.config.schema.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionAttributes;
import org.apache.geode.cache.RegionShortcut;
import org.apache.geode.cache.query.Index;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import org.springframework.data.gemfire.config.schema.SchemaObjectType;
import org.springframework.data.gemfire.config.schema.definitions.RegionDefinition;

/**
 * Unit tests for {@link RegionDefiner}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.apache.geode.cache.Region
 * @see org.springframework.data.gemfire.config.schema.definitions.RegionDefinition
 * @since 2.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class RegionDefinerUnitTests {

	@Mock
	private Index mockIndex;

	@Mock
	private Region<Object, Object> mockRegion;

	private RegionDefiner regionInstanceHandler = new RegionDefiner();

	@Before
	public void setup() {
		when(this.mockRegion.getName()).thenReturn("MockRegion");
	}

	@Test
	public void canHandleIndexInstanceIsFalse() {
		assertThat(this.regionInstanceHandler.canDefine(this.mockIndex)).isFalse();
	}

	@Test
	public void canHandleNullInstanceIsFalse() {
		assertThat(this.regionInstanceHandler.canDefine((Object) null)).isFalse();
	}

	@Test
	public void canHandleRegionInstanceIsTrue() {
		assertThat(this.regionInstanceHandler.canDefine(this.mockRegion)).isTrue();
	}

	@Test
	public void canHandleIndexTypeIsFalse() {
		assertThat(this.regionInstanceHandler.canDefine(Index.class)).isFalse();
	}

	@Test
	public void canHandleNullTypeIsFalse() {
		assertThat(this.regionInstanceHandler.canDefine((Class<?>) null)).isFalse();
	}

	@Test
	public void canHandleRegionTypeIsTrue() {
		assertThat(this.regionInstanceHandler.canDefine(Region.class)).isTrue();
	}

	@Test
	public void canHandleIndexSchemaObjectTypeIsFalse() {
		assertThat(this.regionInstanceHandler.canDefine(SchemaObjectType.INDEX)).isFalse();
	}

	@Test
	public void canHandleNullSchemaObjectTypeIsFalse() {
		assertThat(this.regionInstanceHandler.canDefine((SchemaObjectType) null)).isFalse();
	}

	@Test
	public void canHandleRegionSchemaObjectTypeIsTrue() {
		assertThat(this.regionInstanceHandler.canDefine(SchemaObjectType.REGION)).isTrue();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void defineClientRegion() {

		RegionAttributes<Object, Object> mockRegionAttributes = mock(RegionAttributes.class);

		when(this.mockRegion.getAttributes()).thenReturn(mockRegionAttributes);
		when(mockRegionAttributes.getPoolName()).thenReturn("TestPool");

		RegionDefinition regionDefinition = this.regionInstanceHandler.define(this.mockRegion).orElse(null);

		assertThat(regionDefinition).isNotNull();
		assertThat(regionDefinition.getName()).isEqualTo(this.mockRegion.getName());
		assertThat(regionDefinition.getRegionShortcut()).isEqualTo(RegionShortcut.PARTITION);
		assertThat(regionDefinition.getType()).isEqualTo(SchemaObjectType.REGION);
	}

	@Test
	@SuppressWarnings("unchecked")
	public void definePeerRegion() {

		RegionAttributes<Object, Object> mockRegionAttributes = mock(RegionAttributes.class);

		when(this.mockRegion.getAttributes()).thenReturn(mockRegionAttributes);
		when(mockRegionAttributes.getPoolName()).thenReturn("  ");

		assertThat(this.regionInstanceHandler.define(this.mockRegion).isPresent()).isFalse();
	}

	@Test
	public void defineWithNull() {
		assertThat(this.regionInstanceHandler.define(null).isPresent()).isFalse();
	}
}
