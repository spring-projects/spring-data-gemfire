/*
 * Copyright 2017-2019 the original author or authors.
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

package org.springframework.data.gemfire;

import java.util.Optional;

import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.RegionShortcut;
import org.springframework.core.convert.converter.Converter;
import org.springframework.lang.Nullable;

/**
 * Spring {@link Converter} to convert a {@link RegionShortcut} into a {@link DataPolicy}.
 *
 * @author John Blum
 * @see org.apache.geode.cache.DataPolicy
 * @see org.apache.geode.cache.RegionShortcut
 * @see org.springframework.core.convert.converter.Converter
 * @see org.springframework.data.gemfire.RegionShortcutWrapper
 * @since 2.0.2
 */
public class RegionShortcutToDataPolicyConverter implements Converter<RegionShortcut, DataPolicy> {

	public static final RegionShortcutToDataPolicyConverter INSTANCE = new RegionShortcutToDataPolicyConverter();

	/**
	 * Converts the given {@link RegionShortcut} into a corresponding {@link DataPolicy}.
	 *
	 * @param regionShortcut {@link RegionShortcut} to convert.
	 * @return a corresponding {@link DataPolicy} for the given {@link RegionShortcut}.
	 * @see org.apache.geode.cache.RegionShortcut
	 * @see org.apache.geode.cache.DataPolicy
	 */
	@Nullable @Override
	public DataPolicy convert(RegionShortcut regionShortcut) {

		return Optional.ofNullable(RegionShortcutWrapper.valueOf(regionShortcut))
			.map(RegionShortcutWrapper::getDataPolicy)
			.orElse(DataPolicy.DEFAULT);
	}
}
