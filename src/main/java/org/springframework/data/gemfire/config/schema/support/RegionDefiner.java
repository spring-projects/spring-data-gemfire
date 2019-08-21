/*
 * Copyright 2017-2019 the original author or authors.
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

import static org.springframework.data.gemfire.util.CollectionUtils.asSet;

import java.util.Optional;
import java.util.Set;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionShortcut;

import org.springframework.data.gemfire.config.schema.SchemaObjectDefiner;
import org.springframework.data.gemfire.config.schema.SchemaObjectType;
import org.springframework.data.gemfire.config.schema.definitions.RegionDefinition;
import org.springframework.data.gemfire.util.RegionUtils;

/**
 * The {@link {RegionDefiner} class is responsible for defining a {@link Region}
 * given a reference to a {@link Region} instance.
 *
 * @author John Blum
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.RegionAttributes
 * @see org.apache.geode.cache.RegionShortcut
 * @see org.springframework.data.gemfire.config.schema.SchemaObjectDefiner
 * @see org.springframework.data.gemfire.config.schema.SchemaObjectType
 * @see org.springframework.data.gemfire.config.schema.definitions.RegionDefinition
 * @since 2.0.0
 */
public class RegionDefiner implements SchemaObjectDefiner {

	private final RegionShortcut regionShortcut;

	public RegionDefiner() {
		this(RegionDefinition.DEFAULT_REGION_SHORTCUT);
	}

	public RegionDefiner(RegionShortcut regionShortcut) {
		this.regionShortcut = regionShortcut;
	}

	protected RegionShortcut getRegionShortcut() {
		return Optional.ofNullable(this.regionShortcut).orElse(RegionDefinition.DEFAULT_REGION_SHORTCUT);
	}

	@Override
	public Set<SchemaObjectType> getSchemaObjectTypes() {
		return asSet(SchemaObjectType.REGION);
	}

	@Override
	public Optional<RegionDefinition> define(Object schemaObject) {

		return Optional.ofNullable(schemaObject)
			.filter(this::canDefine)
			.map(it -> (Region) it)
			.filter(RegionUtils::isClient)
			.map(it -> RegionDefinition.from(it).having(getRegionShortcut()));
	}
}
