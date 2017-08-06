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

package org.springframework.data.gemfire.config.schema.definitions;

import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalArgumentException;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.Optional;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionShortcut;
import org.springframework.data.gemfire.config.admin.GemfireAdminOperations;
import org.springframework.data.gemfire.config.schema.SchemaObjectDefinition;
import org.springframework.data.gemfire.config.schema.SchemaObjectType;
import org.springframework.util.StringUtils;

/**
 * {@link RegionDefinition} is an Abstract Data Type (ADT) encapsulating the configuration meta-data used to
 * define a new Apache Geode /  Pivotal GemFire cache {@link Region}.
 *
 * @author John Blum
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.RegionShortcut
 * @see org.springframework.data.gemfire.config.admin.GemfireAdminOperations
 * @see org.springframework.data.gemfire.config.schema.SchemaObjectDefinition
 * @see org.springframework.data.gemfire.config.schema.SchemaObjectType
 * @since 2.0.0
 */
public class RegionDefinition extends SchemaObjectDefinition {

	protected static final int ORDER = 1;

	public static final RegionShortcut DEFAULT_REGION_SHORTCUT = RegionShortcut.PARTITION;

	/**
	 * Factory method used to construct a new instance of {@link RegionDefinition} defined from
	 * the given {@link Region}.
	 *
	 * @param region {@link Region} from which the new definition will be defined.
	 * @return a new instance of {@link RegionDefinition} defined from the given {@link Region}.
	 * @throws IllegalArgumentException if {@link Region} is {@literal null}.
	 * @see org.apache.geode.cache.Region
	 * @see #RegionDefinition(Region)
	 */
	public static RegionDefinition from(Region<?, ?> region) {
		return Optional.ofNullable(region).map(RegionDefinition::new)
			.orElseThrow(() -> newIllegalArgumentException("Region is required"));
	}

	private final transient Region<?, ?> region;

	private RegionShortcut regionShortcut;

	private String name;

	/**
	 * Constructs a new instance of {@link RegionDefinition} defined with the given {@link Region}.
	 *
	 * @param region {@link Region} on which this definition is defined; must not be {@literal null}.
	 * @throws IllegalArgumentException if {@link Region} is {@literal null}.
	 * @see org.apache.geode.cache.Region
	 */
	protected RegionDefinition(Region<?, ?> region) {

		super(Optional.ofNullable(region).map(Region::getName)
			.orElseThrow(() -> newIllegalArgumentException("Region is required")));

		this.region = region;
	}

	/**
	 * Get the order value of this object.
	 *
	 * @return the order value of this object.
	 * @see org.springframework.core.Ordered
	 */
	@Override
	public int getOrder() {
		return ORDER;
	}

	/**
	 * Returns a reference to the {@link Region} from which this definition is defined.
	 *
	 * @return a reference to the {@link Region} from which this definition is defined.
	 * @see org.apache.geode.cache.Region
	 */
	protected Region<?, ?> getRegion() {
		return this.region;
	}

	@Override
	public String getName() {
		return Optional.ofNullable(this.name).filter(StringUtils::hasText).orElseGet(super::getName);
	}

	public RegionShortcut getRegionShortcut() {
		return Optional.ofNullable(this.regionShortcut).orElse(DEFAULT_REGION_SHORTCUT);
	}

	@Override
	public SchemaObjectType getType() {
		return SchemaObjectType.REGION;
	}

	@Override
	public void create(GemfireAdminOperations gemfireAdminOperations) {
		gemfireAdminOperations.createRegion(this);
	}

	public RegionDefinition having(RegionShortcut regionShortcut) {
		this.regionShortcut = regionShortcut;
		return this;
	}

	public RegionDefinition with(String name) {
		this.name = name;
		return this;
	}

	private void writeObject(ObjectOutputStream outputStream) throws IOException {
		outputStream.writeUTF(getName());
		outputStream.writeObject(getRegionShortcut());
	}

	private void readObject(ObjectInputStream inputStream) throws ClassNotFoundException, IOException {
		this.name = inputStream.readUTF();
		this.regionShortcut = (RegionShortcut) inputStream.readObject();
	}
}
